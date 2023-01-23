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

type RiskRewardProfile = 
    {
        Upside : decimal
        Downside : decimal
        RiskRewardRatio : decimal
    }

type CandleStore = Map<string, Candle array>
    
let arrToCandle (arr:decimal array) = 
    {
        OpenTime = arr.[0] |> Decimal.ToUInt64
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
    try 
        candleArray 
        |> Array.sortByDescending (fun x -> x.OpenTime) 
        |> Array.head
        |> Some
    with
        | _ -> None

let latestSymbolDates startDate candleStore availableCoins = 
    let latestStoreSymbolDates = 
        candleStore 
        |> Map.map (fun key value -> 
            match latestCandle value with
                | Some c -> c.OpenTime
                | None -> startDate)
    let initialSymbolDates = 
        availableCoins
        |> Map.map (fun k v -> k, startDate)

    let results = 
        ExtraMap.fullOuterJoin 
            latestStoreSymbolDates 
            initialSymbolDates
        |> Map.map (fun key (left, right) -> 
            match (left, right) with
            | Some l, _ -> l
            | _ -> startDate
        )
    printfn "Latest symbol dates: %A" results
    results

let makeRiskRewardProfile candles =
    try 
        let high = candles |> Array.map (fun x -> x.High) |> Array.max
        let low = candles |> Array.map (fun x -> x.Low) |> Array.min
        let latest = (candles |> Array.last).Close 
        let risk = latest - low
        let reward = high - latest
        {
            Upside = reward / latest
            Downside = risk / latest
            RiskRewardRatio = reward / risk
        } |> Some
    with
        | _ -> None

let mapRiskRewardProfile candleStore =
    candleStore 
    |> Map.map (fun k v -> makeRiskRewardProfile v)
    |> Map.toArray
    |> Array.filter (fun (k, v) -> v <> None)
    |> Array.map (fun (k, v) -> k, v.Value)
    |> Map.ofArray