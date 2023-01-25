module Types 

open System


let second = 1000UL
let minute = 60UL * second
let hour = 60UL * minute
let day = 24UL * hour
let week = 7UL * day


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
    

let binanceArrToCandle (arr:decimal array) = 
    {
        OpenTime = arr.[0] |> Decimal.ToUInt64
        Open = arr.[1]
        High = arr.[2]
        Low = arr.[3]
        Close = arr.[4]
        Volume = arr.[5]
        QuoteAssetVolume = arr.[6]
        TakerBuyBaseAssetVolume = arr.[7]
        TakerBuyQuoteAssetVolume = arr.[8]
    }


let binanceKlinesToCandles (arr:decimal array array) =
    arr |> Array.map binanceArrToCandle


let kucoinArrToCandle (arr: decimal array) =
    {
        OpenTime = arr.[0] * 1000M |> Decimal.ToUInt64
        Open = arr.[1]
        Close = arr.[2]
        High = arr.[3]
        Low = arr.[4]
        Volume = arr.[5]
        QuoteAssetVolume = arr.[6]
        TakerBuyBaseAssetVolume = -1M
        TakerBuyQuoteAssetVolume = -1M
    }


let kucoinKlinesToCandles (arr:decimal array array) =
    arr |> Array.map kucoinArrToCandle


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
            | Some l, _ -> l + day
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