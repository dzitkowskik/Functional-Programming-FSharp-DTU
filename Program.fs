#light

module Project_1 =

(*
A dating bureau has a register containing name, telephone number, sex, year of birth and themes of interest for each client.
You may make a request to the bureau stating your own sex, year of birth and themes of interest and get a response listing
all matching clients, that is, clients with different sex, a deviation in age less than 10 years and with at least one common 
theme of interest. The problem is to construct a program for generating the responses from the dating bureau, where the
response for a matching client comprises name, telephone number and themes of interest for that matching client.
*)

    /// An enumeration type for sex
    type Gender = 
       | Male = 0
       | Female = 1

    /// A type for register record (corresponding the client)
    type Person = {
        Name : string;
        Telephone: string;
        Sex: Gender;
        YearOfBirth : int; 
        Themes : string list
        }

    /// A type for a matching client
    type Matching = {
        Name : string;
        Telephone : string;
        MatchingThemes : string list;
        }

    /// Adds a new client to the register
    let insertToRegister (register:Person list byref) (client:Person) = 
        register <- client::register

    /// Checks if a list contains an item
    let existsIn list item =
        list |> List.exists (fun elem -> elem = item)

    /// Find all similar themes of two clients and returns a pair (tuple) of:
    /// 1) client passed as second parameter
    /// 2) a list of similar themes
    let findSimilaritiesInThemes clientA clientB : (Person*string list) =
        (clientB, clientB.Themes |> List.filter (existsIn clientA.Themes) )

    /// Maps a person object to a Matching object (converts)
    let mapToMatching (client:Person, matchedThemes:string list) =
        { Name = client.Name; Telephone = client.Telephone; MatchingThemes = matchedThemes }

    /// This function finds all the matches for a client (Person obj) in a register (list of Person obj)
    /// Match for a particular client is another client from register with different sex, a deviation in
    /// age less than 10 years and at least one common theme of interest.
    let getResponse register client = 
        register
        |> List.filter (fun x -> x.Sex <> client.Sex)                               // clients with different sex
        |> List.filter (fun x -> abs(x.YearOfBirth - client.YearOfBirth) <= 10)     // a deviation in age less than 10 years
        |> List.map (findSimilaritiesInThemes client)                               // find similarities in themes of interest
        |> List.filter (fun (_,b) -> b.Length > 0)                                  // with at least one common theme of interest
        |> List.map mapToMatching   // response for a matching client comprises name, telephone number and themes of interest


#if DEBUG

    //////////////////////////////
    //           TESTS          //
    //////////////////////////////

    let printTestResult testName result =
        let oldColor = System.Console.ForegroundColor
        if result then 
            System.Console.ForegroundColor <- System.ConsoleColor.Green
            printfn "%s OK" testName
        else
            System.Console.ForegroundColor <- System.ConsoleColor.Red
            printfn "%s FAILED" testName
        System.Console.ForegroundColor <- oldColor


    /////////////////////
    // 1) Preparing data
    /////////////////////

    let mutable Register = [
        {
            Name = "Sabrina";
            Telephone = "666666666";
            Sex = Gender.Female;
            YearOfBirth = 1991;
            Themes = ["Music"; "Sex"]
            };
        {
            Name = "Helga";
            Telephone = "123456789";
            Sex = Gender.Female;
            YearOfBirth = 1940;
            Themes = ["Poetry"; "TV Series"; "Flowers"]
            };
        {
            Name = "Marian";
            Telephone = "987654321";
            Sex = Gender.Male; 
            YearOfBirth = 1989;
            Themes = ["Cars"; "Sex"; "Football"; "Food"; "TV Series"]
            };
        ]

    let ClientA = {
        Name = "Charles";
        Telephone = "111111111";
        Sex = Gender.Male; 
        YearOfBirth = 1987;
        Themes = ["Cars"; "Sex"; "Football"; "Food"; "TV Series"]
        }

    let ClientB = {
        Name = "Kate";
        Telephone = "+48 111222333";
        Sex = Gender.Female; 
        YearOfBirth = 1994;
        Themes = ["Cars"; "Food"; "TV Series"; "Flowers"]
        }

    let ClientC = {
        Name = "Superman";
        Telephone = "777777777";
        Sex = Gender.Male; 
        YearOfBirth = 1985;
        Themes = ["Power"; "Peace"; "Be super"]
        }


    /////////////////
    // 2. Unit tests
    /////////////////

    let insertToRegister_UnitTest = 
        insertToRegister &Register ClientA
        let result = Register.[0]
        let expected = ClientA
        printTestResult "insertToRegister_UnitTest" (result = expected)

    let existsIn_true_UnitTest =
        let result = existsIn Register ClientA
        let expected = true
        printTestResult "existsIn_true_UnitTest" (result = expected)

    let existsIn_false_UnitTest =
        let result = existsIn Register ClientB
        let expected = false
        printTestResult "existsIn_true_UnitTest" (result = expected)

    let findSimilaritiesInThemes_noSimilarity_UnitTest = 
        let result = findSimilaritiesInThemes ClientC ClientB
        let expected = (ClientB, [])
        printTestResult "findSimilaritiesInThemes_noSimilarity_UnitTest" (result = expected)

    let findSimilaritiesInThemes_someSimilarity_UnitTest = 
        let result = findSimilaritiesInThemes ClientB ClientA
        let expected = (ClientA, ["Cars"; "Food"; "TV Series"])
        printTestResult "findSimilaritiesInThemes_someSimilarity_UnitTest" (result = expected)

    let findSimilaritiesInThemes_allSimilar_UnitTest = 
        let result = findSimilaritiesInThemes ClientC ClientC
        let expected = (ClientC, ["Power"; "Peace"; "Be super"])
        printTestResult "findSimilaritiesInThemes_allSimilar_UnitTest" (result = expected)

    let mapToMatching_UnitTest = 
        let mthemes = ["Computer Games"; "TV Series"]
        let result = mapToMatching (ClientA, mthemes)
        let expected = { Name = "Charles"; Telephone = "111111111"; MatchingThemes = mthemes }
        printTestResult "mapToMatching_UnitTest" (result = expected)

    let getResponse_noResults_UnitTest = 
        let result = getResponse Register ClientC
        let expected = []
        printTestResult "getResponse_noResults_UnitTest" (result = expected)

    let getResponse_oneResult_UnitTest = 
        let result = getResponse Register ClientA
        let matching = { Name = "Sabrina"; Telephone = "666666666"; MatchingThemes = ["Sex"] }
        let expected = [matching]
        printTestResult "getResponse_oneResult_UnitTest" (result = expected)

    let getResponse_multipleResults_UnitTest = 
        let result = getResponse Register ClientB
        let mthemes = ["Cars"; "Food"; "TV Series"]
        let matching_1 = { Name = "Charles"; Telephone = "111111111"; MatchingThemes = mthemes } 
        let matching_2 = { Name = "Marian"; Telephone = "987654321"; MatchingThemes = mthemes }
        let expected = [matching_1; matching_2]
        printTestResult "getResponse_multipleResults_UnitTest" (result = expected)

#endif