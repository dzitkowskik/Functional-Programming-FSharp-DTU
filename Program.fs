#light

module Project_1 =

(*
A dating bureau has a register containing name, telephone number, sex, year of birth and themes of interest for each client.
You may make a request to the bureau stating your own sex, year of birth and themes of interest and get a response listing
all matching clients, that is, clients with different sex, a deviation in age less than 10 years and with at least one common 
theme of interest. The problem is to construct a program for generating the responses from the dating bureau, where the
response for a matching client comprises name, telephone number and themes of interest for that matching client.
*)

    type Gender = 
       | Male = 0
       | Female = 1

    type Person = {
        Name : string;
        Telephone: string;
        Sex: Gender;
        YearOfBirth : int; 
        Themes : string list
        }

    type Matching = {
        Name : string;
        Telephone : string;
        MatchingThemes : string list;
        }
    
    let insertToRegister (register:Person list byref) (client:Person) = 
        register <- client::register

    let existsIn list item =
        list |> List.exists (fun elem -> elem = item)

    let findSimilaritiesInThemes clientA clientB : (Person*string list) =
        (clientB, clientB.Themes |> List.filter (existsIn clientA.Themes) )

    let mapToMatching (client:Person, matchedThemes:string list) =
        { Name = client.Name; Telephone = client.Telephone; MatchingThemes = matchedThemes }

    let getResponse register client = 
        register
        |> List.filter (fun x -> x.Sex <> client.Sex)                               // clients with different sex
        |> List.filter (fun x -> abs(x.YearOfBirth - client.YearOfBirth) <= 10)     // a deviation in age less than 10 years
        |> List.map (findSimilaritiesInThemes client)                               // find similarities in themes of interest
        |> List.filter (fun (_,b) -> b.Length > 0)                                  // with at least one common theme of interest
        |> List.map mapToMatching   // response for a matching client comprises name, telephone number and themes of interest

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


    /////////////////
    // 2. Unit tests
    /////////////////

    let insertToRegister_UnitTest = 
        insertToRegister &Register ClientA
        let result = Register.[0]
        let expected = ClientA
        printTestResult "insertToRegister_UnitTest" (result = expected)

    

    let Response = getResponse Register ClientA