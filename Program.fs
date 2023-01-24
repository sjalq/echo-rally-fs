open System
open Types
open JsonFileProcessing
open ExtraMap
open FSharp.Data
open System.Linq
open System.IO
open Delay

// For more information see https://aka.ms/fsharp-console-apps
printfn "Hello from F#"

// fetch overlapping coins
// fetch local data
// update local data from binance
// store local data
// do calculations
// give list of best R/R coins

type BinanceExchangeInfo = JsonProvider<"jsonSamples/binanceExchangeInfo.json">
type KucoinSymbols = JsonProvider<"jsonSamples/kucoinSymbols.json">
type CMCList = JsonProvider<"jsonSamples/cmcList.json">

let binanceExchangeInfo = new StreamReader("jsonSamples/binanceExchangeInfo.json") |> BinanceExchangeInfo.Load
let binanceList = 
    binanceExchangeInfo.Symbols 
    |> Array.filter (fun x -> x.QuoteAsset = "USDT")
    |> Array.map (fun x -> x.BaseAsset, x)
    |> Map.ofArray

let kucoinSymbols = new StreamReader("jsonSamples/kucoinSymbols.json") |> KucoinSymbols.Load
let kucoinList = 
    kucoinSymbols.Data 
    |> Array.filter (fun x -> x.QuoteCurrency = "USDT")
    |> Array.map (fun x -> x.BaseCurrency, x)
    |> Map.ofArray

let cmc = new StreamReader("jsonSamples/cmcList.json") |> CMCList.Load
let cmcList = 
    cmc.Data
    |> Array.sortBy (fun x -> x.JsonValue.["rank"])
    |> Array.take 1000
    |> Array.map (fun x -> x.Symbol, x)
    |> Map.ofArray

let availableCoins =
    ExtraMap.fullOuterJoin 
        binanceList
        kucoinList
    |> ExtraMap.innerJoin cmcList
    |> Map.map (fun k (l, r) -> 0 )


let mutable globalCache = Map.empty<string, Candle array>
globalCache <- deserializeJsonFile "data/globalCache.json"
let march_1_2022 = 1646170171000UL // The start of our interested period

printfn "Available Coins: %A" availableCoins
let symbolDatesToFetch  = 
    latestSymbolDates march_1_2022 globalCache availableCoins
    // |> ExtraMap.take 10

type BinanceCandleInfo = JsonProvider<"jsonSamples/binancePriceHistory.json">

let fetchBinanceCandles symbol startDate =
    printfn "Binance : Fetching %s from %A" symbol startDate
    let url = 
        String.Format(
            "https://api.binance.com/api/v3/klines?symbol={0}&interval=1d&startTime={1}"
            , symbol + "USDT"
            , startDate
        )
    BinanceCandleInfo.Load(url)
    |> binanceKlinesToCandles

type KucoinCandleInfo = JsonProvider<"jsonSamples/kucoinKlines.json">

let delayPeriod = 300
let mutable lastTimeCalled = None
let fetchKucoinCandles symbol startDate =
    lastTimeCalled <- delay delayPeriod lastTimeCalled
    printfn "Kucoin : Fetching %s from %A" symbol startDate
    let url = 
        String.Format(
            "https://api.kucoin.com/api/v1/market/candles?symbol={0}&type=1day&startAt={1}"
            , symbol + "-USDT"
            , startDate / 1000UL
        )
    KucoinCandleInfo.Load(url).Data
    |> kucoinKlinesToCandles

let fetchNewData symbolDatesToFetch (exchangeList:Map<string,'b>) updateFunction allowParallel =
    symbolDatesToFetch
        |> Map.filter (fun symbol _ -> exchangeList.ContainsKey symbol)
        |> Map.toArray
        |> (if allowParallel then Array.Parallel.map else Array.map) 
            (fun (symbol, startDate) -> symbol, updateFunction symbol startDate)
        |> Map.ofArray

let updateCache cache dataToAdd =
    ExtraMap.fullOuterJoin
        cache
        dataToAdd
    |> Map.map (fun key (left, right) ->
        match (left, right) with
        | Some l, Some r -> 
            l |> Array.append r |> Array.sortBy (fun x -> x.OpenTime)
        | Some l, None -> l
        | None, Some r -> r
        | None, None -> failwith "This should never happen"
    )

//let binanceUpdate = fetchMissingData symbolDatesToFetch binanceList fetchBinanceCandles
let kucoinUpdate = fetchNewData symbolDatesToFetch kucoinList fetchKucoinCandles false
let fullUpdate = kucoinUpdate // ExtraMap.merge binanceUpdate kucoinUpdate
let newCache = updateCache globalCache fullUpdate

newCache |> serializeJsonFile "data/globalCache.json"

globalCache <- newCache

let results = 
    globalCache 
    |> mapRiskRewardProfile
    |> rankMultipleUnified 
        [|
            (fun x -> (x.Upside)), Descending
            //(fun x -> (x.Downside)), Ascending
            //(fun x -> (x.RiskRewardRatio)), Descending
        |]
        manhattanDistance
    |> Array.map (fun (k, (v, r)) -> sprintf "%s: Downside:%A Upside:%A Rank:%A" k v.Downside v.Upside r)

File.WriteAllLines("data/rankings.txt", results)


[<EntryPoint>]
let main argv =
    printfn "Hello World!"
    0 // return an integer exit code