open System
open Types
open JsonFileProcessing
open ExtraMap
open FSharp.Data
open System.Linq

// For more information see https://aka.ms/fsharp-console-apps
printfn "Hello from F#"

// fetch overlapping coins
// fetch local data
// update local data from binance
// store local data
// do calculations
// give list of best R/R coins

type ExchangeInfo = JsonProvider<"jsonSamples/binanceExchangeInfo.json">
type CMCList = JsonProvider<"jsonSamples/cmcList.json">

let availableCoins = 
    let binanceExchangeInfo = ExchangeInfo.Load("jsonSamples/binanceExchangeInfo.json")
    let binanceList = binanceExchangeInfo.Symbols |> Array.map (fun x -> x) |> Array.filter (fun x -> x.QuoteAsset = "USDT")

    let cmc = CMCList.Load("cmcList.json")
    let cmcList = 
        cmc.Data
        |> Array.sortBy (fun x -> x.JsonValue.["rank"])
        |> Array.take 500

    Enumerable.Join
        ( binanceList
        , cmcList
        , (fun b -> b.QuoteAsset)
        , (fun c -> c.Symbol)
        , (fun b c -> b.Symbol, c)
        )
    |> Seq.sortBy (fun (b,c) -> c.JsonValue.["rank"])

let mutable globalCache = Map.empty<string, Candle array>
globalCache <- deserializeJsonFile "data/globalCache.json"
let march_1_2022 = 1646170171000UL // The start of our interested period

let symbolDatesToFetch  = 
    latestSymbolDates march_1_2022 globalCache availableCoins
    |> ExtraMap.take 10

type CandleInfo = JsonProvider<"jsonSamples/binancePriceHistory.json">

let fetchCandles symbol startDate =
    printfn "Fetching %s from %A" symbol startDate
    let url = 
        String.Format(
            "https://api.binance.com/api/v3/klines?symbol={0}&interval=1h&startTime={1}"
            , symbol
            , startDate
        )
    CandleInfo.Load(url)
    |> klinesToCandles

let fetchMissingData symbolDatesToFetch =
    symbolDatesToFetch |> Map.map fetchCandles 

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

let newCache = updateCache globalCache (fetchMissingData symbolDatesToFetch)

newCache |> serializeJsonFile "data/globalCache.json"