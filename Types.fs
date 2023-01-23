module Types 

open System

let nowString () = 
    DateTime.Now.ToString("yyyy-MM-dd HH-mm-ss")

type Candle = 
    { 
        OpenTime : uint64
        Open : decimal
        High : decimal
        Low : decimal
        Close : decimal
        Volume : decimal
        QuoteAssetVolume : decimal
        TakerBuyBaseAssetVolume : decimal
        TakerBuyQuoteAssetVolume : decimal
    }

type CandleStore = Map<string, Candle array>
    
let arrToCandle (arr:decimal array) = 
    {
        OpenTime = arr.[0] |> Decimal.ToInt64
        Open = arr.[1]
        High = arr.[1]
        Low = arr.[1]
        Close = arr.[1]
        Volume = arr.[1]
        QuoteAssetVolume = arr.[1]
        TakerBuyBaseAssetVolume = arr.[1]
        TakerBuyQuoteAssetVolume = arr.[1]
    }

let klinesToCandles (arr:decimal array array) =
    arr |> Array.map arrToCandle

let latestCandle candleArray = 
    candleArray 
    |> Array.sortByDescending (fun x -> x.OpenTime) 
    |> Array.head

let latestSymbolDates startDate candleStore availableCoins = 
    let latestStoreSymbolDates = 
        candleStore 
        |> Map.map (fun key value -> (latestCandle value).OpenTime)
    let initialSymbolDates = 
        availableCoins
        |> Seq.map (fun (b,c) -> b, startDate)
        |> Map.ofSeq

    ExtraMap.fullOuterJoin 
        latestStoreSymbolDates 
        initialSymbolDates
    |> Map.map (fun key (left, right) -> 
        match (left, right) with
        | Some l, _ -> l
        | _ -> startDate
    )
