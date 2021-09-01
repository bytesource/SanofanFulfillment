


// Sfc: Handle theses problems:

// USPE doesn't ship to Hawaii, Alaska
// ("Remote areas, no service" OR "Province/State no service No service for this post code")

// When customer selects Isle of Man as the country, which SFC
// has no route to, we must do the following:
// 1) Select country: UK
// 2) Select state: Isle of Man
// 3) Select shipping method: STEXPTH (using Royal Mail)

// Address length: Royal Mail: Address required with a minimal character length
//       (house number not enough).
// Zip format: US customers sometimes write zip code in the format 12345-1234, 
//       which DHL doesn't accept.