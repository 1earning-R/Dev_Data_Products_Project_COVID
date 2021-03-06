State_Codes <-c("United States" = "US",
                "Alabama" = "US-AL",
                "Alaska" = "US-AK",
                "Arizona" = "US-AZ",
                "Arkansas" = "US-AR",
                "California" = "US-CA",
                "Colorado" = "US-CO",
                "Connecticut" = "US-CT",
                "Delaware" = "US-DE",
                "District of Columbia" = "US-DC",
                "Florida" = "US-FL",
                "Georgia" = "US-GA",
                "Hawaii" = "US-HI",
                "Idaho" = "US-ID",
                "Illinois" = "US-IL",
                "Indiana" = "US-IN",
                "Iowa" = "US-IA",
                "Kansas" = "US-KS",
                "Kentucky" = "US-KY",
                "Louisiana" = "US-LA",
                "Maine" = "US-ME",
                "Maryland" = "US-MD",
                "Massachusetts" = "US-MA",
                "Michigan" = "US-MI",
                "Minnesota" = "US-MN",
                "Mississippi" = "US-MS",
                "Missouri" = "US-MO",
                "Montana" = "US-MT",
                "Nebraska" = "US-NE",
                "Nevada" = "US-NV",
                "New Hampshire" = "US-NH",
                "New Jersey" = "US-NJ",
                "New Mexico" = "US-NM",
                "New York" = "US-NY",
                "North Carolina" = "US-NC",
                "North Dakota" = "US-ND",
                "Ohio" = "US-OH",
                "Oklahoma" = "US-OK",
                "Oregon" = "US-OR",
                "Pennsylvania" = "US-PA",
                "Rhode Island" = "US-RI",
                "South Carolina" = "US-SC",
                "South Dakota" = "US-SD",
                "Tennessee" = "US-TN",
                "Texas" = "US-TX",
                "Utah" = "US-UT",
                "Vermont" = "US-VT",
                "Virginia" = "US-VA",
                "Washington" = "US-WA",
                "West Virginia" = "US-WV",
                "Wisconsin" = "US-WI",
                "Wyoming" = "US-WY")

State_Abb <-data.frame(rbind(c("United States", "US"),
                             c("United States", "U.S."),
                             c("Alabama", "AL"),
                             c("Alaska", "AK"),
                             c("Arizona", "AZ"),
                             c("Arkansas", "AR"),
                             c("California", "CA"),
                             c("Colorado", "CO"),
                             c("Connecticut", "CT"),
                             c("Delaware", "DE"),
                             c("District of Columbia", "DC"),
                             c("District of Columbia", "D.C."),
                             c("Florida", "FL"),
                             c("Georgia", "GA"),
                             c("Hawaii", "HI"),
                             c("Idaho", "ID"),
                             c("Illinois", "IL"),
                             c("Indiana", "IN"),
                             c("Iowa", "IA"),
                             c("Kansas", "KS"),
                             c("Kentucky", "KY"),
                             c("Louisiana", "LA"),
                             c("Maine", "ME"),
                             c("Maryland", "MD"),
                             c("Massachusetts", "MA"),
                             c("Michigan", "MI"),
                             c("Minnesota", "MN"),
                             c("Mississippi", "MS"),
                             c("Missouri", "MO"),
                             c("Montana", "MT"),
                             c("Nebraska", "NE"),
                             c("Nevada", "NV"),
                             c("New Hampshire", "NH"),
                             c("New Jersey", "NJ"),
                             c("New Mexico", "NM"),
                             c("New York", "NY"),
                             c("North Carolina", "NC"),
                             c("North Dakota", "ND"),
                             c("Ohio", "OH"),
                             c("Oklahoma", "OK"),
                             c("Oregon", "OR"),
                             c("Pennsylvania", "PA"),
                             c("Rhode Island", "RI"),
                             c("South Carolina", "SC"),
                             c("South Dakota", "SD"),
                             c("Tennessee", "TN"),
                             c("Texas", "TX"),
                             c("Utah", "UT"),
                             c("Vermont", "VT"),
                             c("Virginia", "VA"),
                             c("Washington", "WA"),
                             c("West Virginia", "WV"),
                             c("Wisconsin", "WI"),
                             c("Wyoming", "WY")))

Neighbors <-list("US-AL" = c("US-AL","US-TN", "US-MS","US-FL","US-GA"),
                 "US-AK" = c("US-AK"),
                 "US-AZ" = c("US-AZ","US-NV","US-UT","US-NM","US-CA","US-CO"),
                 "US-AR" = c("US-AR","US-TX","US-OK","US-MO","US-TN","US-MS","US-LA"),
                 "US-CA" = c("US-CA","US-OR","US-AZ","US-NV"),
                 "US-CO" = c("US-CO","US-NM","US-OK","US-KS","US-NE","US-WY","US-UT","US-AZ"),
                 "US-CT" = c("US-CT","US-NY","US-MA","US-RI"),
                 "US-DE" = c("US-DE","US-MD","US-PA","US-NJ"),
                 "US-DC" = c("US-DC","US-VA","US-MD"),
                 "US-FL" = c("US-FL","US-GA","US-AL"),
                 "US-GA" = c("US-GA","US-FL","US-AL","US-TN","US-NC","US-SC"),
                 "US-HI" = c("US-HI"),
                 "US-ID" = c("US-ID","US-WA","US-OR","US-NV","US-UT","US-WY","US-MT"),
                 "US-IL" = c("US-IL","US-KY","US-MO","US-IA","US-WI","US-IN"),
                 "US-IN" = c("US-IN","US-IL","US-OH","US-MI","US-KY"),
                 "US-IA" = c("US-IA","US-MO","US-NE","US-SD","US-MN","US-WI","US-IL"),
                 "US-KS" = c("US-KS","US-NE","US-CO","US-OK","US-MO"),
                 "US-KY" = c("US-KY","US-MO","US-IL","US-IN","US-OH","US-WV","US-VA","US-TN"),
                 "US-LA" = c("US-LA","US-MS","US-AR","US-TX"),
                 "US-ME" = c("US-ME","US-NH"),
                 "US-MD" = c("US-MD","US-WV","US-VA","US-DC","US-PA","US-DE"),
                 "US-MA" = c("US-MA","US-NY","US-VT","US-NH","US-CT","US-RI"),
                 "US-MI" = c("US-MI","US-WI","US-IN","US-OH"),
                 "US-MN" = c("US-MN","US-ND","US-SD","US-IA","US-WI"),
                 "US-MS" = c("US-MS","US-AL","US-LA","US-TN","US-AR"),
                 "US-MO" = c("US-MO","US-OK","US-KS","US-NE","US-IA","US-IL","US-KY","US-TN","US-AR"),
                 "US-MT" = c("US-MT","US-ID","US-WY","US-ND","US-SD"),
                 "US-NE" = c("US-NE","US-KS","US-IA","US-SD","US-WY","US-CO","US-MO"),
                 "US-NV" = c("US-NV","US-CA","US-OR","US-ID","US-UT","US-AZ"),
                 "US-NH" = c("US-NH","US-ME","US-MA","US-VT"),
                 "US-NJ" = c("US-NJ","US-PA","US-NY","US-DE"),
                 "US-NM" = c("US-NM","US-AZ","US-TX","US-OK","US-CO","US-UT"),
                 "US-NY" = c("US-NY","US-NJ","US-PA","US-MA","US-VT","US-CT"),
                 "US-NC" = c("US-NC","US-VA","US-SC","US-TN","US-GA"),
                 "US-ND" = c("US-ND","US-SD","US-MN","US-MT"),
                 "US-OH" = c("US-OH","US-MI","US-WV","US-IN","US-KY","US-PA"),
                 "US-OK" = c("US-OK","US-TX","US-MO","US-AR","US-NM","US-CO","US-KS"),
                 "US-OR" = c("US-OR","US-CA","US-WA","US-NV","US-ID"),
                 "US-PA" = c("US-PA","US-OH","US-NY","US-NJ","US-DE","US-MD","US-WV"),
                 "US-RI" = c("US-RI","US-MA","US-CT"),
                 "US-SC" = c("US-SC","US-NC","US-GA"),
                 "US-SD" = c("US-SD","US-ND","US-MN","US-MT","US-IA","US-NE","US-WY"),
                 "US-TN" = c("US-TN","US-KY","US-NC","US-VA","US-GA","US-AL","US-MS","US-AR","US-MO"),
                 "US-TX" = c("US-TX","US-NM","US-OK","US-AR","US-LA"),
                 "US-UT" = c("US-UT","US-CO","US-AZ","US-NV","US-NM","US-ID","US-WY"),
                 "US-VT" = c("US-VT","US-NH","US-MA","US-NY"),
                 "US-VA" = c("US-VA","US-WV","US-MD","US-DC","US-NC","US-KY","US-TN"),
                 "US-WA" = c("US-WA","US-OR","US-ID"),
                 "US-WV" = c("US-WV","US-PA","US-MD","US-OH","US-KY","US-VA"),
                 "US-WI" = c("US-WI","US-MI","US-MN","US-IA","US-IL"),
                 "US-WY" = c("US-WY","US-CO","US-UT","US-ID","US-MT","US-SD","US-NE"))