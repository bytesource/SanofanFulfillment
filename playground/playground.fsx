
// ======================================
// Types
// ======================================

// 1) From Json
// type RawOrder
// All raw types as a record

// 2) SFC verification
// type WarehouseOrder

// 3) PayPal (if we need other )
// type PayPalOrde


type Quality =
    private { Value' : byte }

    static member Of (number: int) = 
        { Value' = byte number }

    static member Of (number: string) = 
        { Value' = byte 3 }
    
    override me.ToString() = 
        $"%02i{me.Value'}"

    // NOTE other behavior elided

    static member op_Explicit(quality : Quality) : int = 
        int quality.Value'

let quality = Quality.Of(7)
printfn $"%s{string quality}, or %i{int quality}"
// 07, or 7


let quality2 = Quality.Of("dummy")
printfn $"%s{string quality2}, or %i{int quality2}"
// 03, or 3