open System
open Types
open JsonFileProcessing
open ExtraMap
open FSharp.Data
open System.Linq
open System.IO

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
    let streamReader = new StreamReader("jsonSamples/binanceExchangeInfo.json")
    let binanceExchangeInfo = ExchangeInfo.Load(streamReader)
    let binanceList = 
        binanceExchangeInfo.Symbols 
        |> Array.filter (fun x -> x.QuoteAsset = "USDT")
        |> Array.map (fun x -> x.BaseAsset, x)
        |> Map.ofArray
    // printfn "BinanceList %A" binanceList.Count

    let streamReader = new StreamReader("jsonSamples/cmcList.json")
    let cmc = CMCList.Load(streamReader)
    let cmcList = 
        cmc.Data
        |> Array.sortBy (fun x -> x.JsonValue.["rank"])
        |> Array.take 100
        |> Array.map (fun x -> x.Symbol, x)
        |> Map.ofArray
    // printfn "CMCList %A" cmcList

    ExtraMap.innerJoin
        binanceList
        cmcList
    |> Map.map (fun k (l, r) -> 0 )


let mutable globalCache = Map.empty<string, Candle array>
globalCache <- deserializeJsonFile "data/globalCache.json"
let march_1_2022 = 1646170171000UL // The start of our interested period

printfn "Available Coins: %A" availableCoins
let symbolDatesToFetch  = 
    latestSymbolDates march_1_2022 globalCache availableCoins
    // |> ExtraMap.take 10

type CandleInfo = JsonProvider<"jsonSamples/binancePriceHistory.json">

let fetchCandles symbol startDate =
    printfn "Fetching %s from %A" symbol startDate
    let url = 
        String.Format(
            "https://api.binance.com/api/v3/klines?symbol={0}&interval=1d&startTime={1}"
            , symbol
            , startDate
        )
    CandleInfo.Load(url)
    |> klinesToCandles

let fetchMissingData symbolDatesToFetch =
    symbolDatesToFetch 
    |> Map.toArray
    |> Array.Parallel.map (fun (symbol, startDate) -> symbol, fetchCandles symbol startDate)
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

let newCache = updateCache globalCache (fetchMissingData symbolDatesToFetch)

newCache |> serializeJsonFile "data/globalCache.json"

globalCache <- newCache

// find the coins with the best R/R between their highs, lows and current price
// globalCache 
// |> Map.map (fun k v -> makeRiskRewardProfile v)
// |> Map.toArray
// |> Array.filter (fun (k, v) -> v <> None)
// |> Array.map (fun (k, v) -> k, v.Value)
// |> Array.sortByDescending (fun (k, v) -> v.RiskRewardRatio)
// |> Array.take 100
// |> Array.iter (fun (k, v) -> printfn "%s: RR:%A Upside:%A" k v.RiskRewardRatio v.Upside)

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