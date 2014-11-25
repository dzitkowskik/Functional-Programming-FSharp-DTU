#light

//////////////////////////////////////////
//                                      //
//  Karol Dzitkowski s142246 DTU        //
//  03.11.2014                          //
//  k.dzitkowski@gmail.com              //
//                                      //
//////////////////////////////////////////

module Assignment_4 =

    open System

    //////////////////////////////////////////////////////
    /////////////   Problem 3 Exam May 2014    ///////////
    //////////////////////////////////////////////////////

    // Types

    type Name = string

    type Sex = 
        | M
        | F

    type YearOfBirth = int

    type FamilyTree = P of Name * Sex * YearOfBirth * Children
    and Children = FamilyTree list


    // Help functions

    let getYear = function
        | P(_,_,year,_) -> year

    let getName = function
        | P(name,_,_,_) -> name

    let children_info children = 
        children
        |> List.fold (fun acc x -> (getName x)::acc) []
        |> List.rev

    let str = function
        | M -> "Male"
        | F -> "Female"

    let insert c childr =
        c::childr |> List.sortBy (fun x -> getYear x)

    // Solutions

    (*
    Declare a function isWF: FamilyTree -> bool that can check whether
    a family tree iw well-formed - has two properties:
    1. Every person must be older than his/her children
    2. For every list of children, the siblings occur with decreasing 
       ages so that the eldest occurs first and the youngest last.
    *)


    let rec isWF = function
        | P(_, _, year, children) -> 
            children |> Seq.pairwise |> Seq.forall (fun (a, b) -> getYear a <= getYear b)
            && children |> List.forall(fun x -> getYear x > year && isWF x)

    (*
    Declare a function makePerson: Name*Sex*YearOfBirth -> FamilyTree
    that can create family tree for a child-less person.
    *)

    let makePerson (name, sex, year) = P(name, sex, year, [])
       
    (*
    Declare two mutually recursive functions insertChildOf and insertChildOfInList
    that creates a new tree from existing tree by adding child of person with some
    name. If it can't be done or if such a tree would not be well-formed return None.
    *)

    let rec insertChildOf n c t =
        match t with
        | P(name, sex, year, children) ->
            if name=n then 
                let temp = P(name, sex, year, insert c children)
                if isWF(temp) then Some(temp) else None
            else 
                match insertChildOfInList n c children with 
                | Some(x) ->
                    let temp = P(name, sex, year, x)
                    if isWF(temp) then Some(temp) else None
                | None -> None
    and insertChildOfInList n c children =
        match children with
        | [] -> None
        | head::tail -> 
            match insertChildOf n c head with 
            | Some(x) -> Some(x::tail)
            | None -> 
                match insertChildOfInList n c tail with
                | Some(y) -> Some(head::y)
                | None -> None

    (*
    Declare a function find so that find n t extracts information about the person
    named n in the family t. This info comprises sex, year of birth and the names
    of all children of that person.
    *)

    let rec find n t = 
        match t with
        | P(name, sex, year, children) ->
            if name=n then Some((sex, year, children_info children))
            else find_list n children
    and find_list n c =
        match c with
        | [] -> None
        | head::tail -> 
            match find n head with
            | Some(x) -> Some(x)
            | None -> find_list n tail
            
    (*
    Declare a function toString n t that gives a string representation for the family t
    using n blank characters as indentation between generation.
    *)
       
    // returns string
    let toString n tree =
        let tab = String.replicate n " "
        let rec toString ident = function    
            | P(name,sex,year,children) ->
                let text = ident + name + " " + str(sex) + " " + year.ToString() + "\n"
                List.fold (fun acc elem -> acc + toString (ident+tab) elem) text children
        toString "" tree

    // just prints out the tree
    let printOut n tree = 
        let tab = String.replicate n " "
        let rec printOut ident = function
            | P(name,sex,year,children) ->
                printf "%s%s %s %d\n" ident name (str(sex)) year
                List.forall (fun x -> printOut (ident+tab) x) children
        printOut "" tree

    (*
    Declare a function truncate that produces restricted family tree from a given family tree.
    *)

    let rec truncate = function
        | P(name,sex,year,children) -> 
            match sex with
            | F -> P(name, sex, year, [])
            | M -> P(name, sex, year, List.map (fun x -> truncate(x)) children)


    //////////////////////////////
    //           TESTS          //
    //////////////////////////////

    // 1. Helper functions

    let rec compareTrees = function
        | (P(n1, s1, y1, c1), P(n2, s2, y2, c2)) ->
            if  n1 = n2 &&
                s1 = s2 &&
                y1 = y2 &&
                List.forall2 (fun e1 e2 -> compareTrees(e1, e2)) c1 c2 
            then true else false

    let printTestResult testName result =
        let oldColor = System.Console.ForegroundColor
        if result then 
            System.Console.ForegroundColor <- System.ConsoleColor.Green
            printfn "%s OK" testName
        else
            System.Console.ForegroundColor <- System.ConsoleColor.Red
            printfn "%s FAILED" testName
        System.Console.ForegroundColor <- oldColor

    type List<'a> with 
       static member existsIn list item =
        list |> List.exists (fun elem -> elem = item)

    let compareLists listA listB = 
        listA |> List.forall (fun elem -> List.existsIn listB elem)

    // 2. Preparing data


    // a)
    let MaryChildren = [
        P("Peter", M, 2005, []);
        P("Bob", M, 2008, []);
        P("Eve", F, 2010, [])]
    let Marry = P("Marry", F, 1980, MaryChildren)

    let MayChildren = [
        P("Fred", M, 1970, []);
        P("Joan", F, 1975, [])]
    let May = P("May", F, 1945, MayChildren)

    let JoeChildren = [
        P("Stanley", M, 1975, []);
        Marry;
        P("Jane", F, 1985, [])]
    let Joe = P("Joe", M, 1950, JoeChildren)

    let Paul = P("Paul", M, 1955, [])
    let LarryChildren = [May; Joe; Paul]
    let Larry = P("Larry", M, 1920, LarryChildren)

    // b)
    let JacksonChildren = [Larry; Joe; Paul]
    let Jackson = P("Jackson", M, 1930, JacksonChildren)

    let CharlesChildren = [Jackson; May; Marry]
    let Charles = P("Charles", M, 1910, CharlesChildren)

    let JacksonChildren2 = [Joe; Paul]
    let Jackson2 = P("Jackson", M, 1930, JacksonChildren2)

    let CharlesChildren2 = [Jackson2; May; Marry]
    let Charles2 = P("Charles", M, 1910, CharlesChildren2)

    let JacksonChildren3 = [Paul; Joe]
    let Jackson3 = P("Jackson", M, 1930, JacksonChildren3)

    let CharlesChildren3 = [Jackson3; May; Marry]
    let Charles3 = P("Charles", M, 1910, CharlesChildren3)

    // c)
    let MaryChildren_expected = [
        P("Peter", M, 2005, []);
        P("Bob", M, 2008, []);
        P("Eve", F, 2010, [])]
    let Marry_expected = P("Marry", F, 1980, MaryChildren_expected)

    let MayChildren_expected = [
        P("Fred", M, 1970, []);
        P("Joan", F, 1975, [])]
    let May_expected = P("May", F, 1945, MayChildren_expected)

    let JoeChildren_expected = [
        P("Stanley", M, 1975, []);
        Marry_expected;
        P("Jane", F, 1985, [])]
    let Joe_expected = P("Joe", M, 1950, JoeChildren_expected)
    let Mirka_expected = P("Mirka", F, 1990, [])
    let Paul_expected = P("Paul", M, 1955, [Mirka_expected])
    let LarryChildren_expected = [May_expected; Joe_expected; Paul_expected]
    let Larry_expected = P("Larry", M, 1920, LarryChildren_expected)

    // d)
    let MaryChildren_expected2 = [
        P("Peter", M, 2005, []);
        P("Bob", M, 2008, []);
        P("Eve", F, 2010, [])]
    let Marry_expected2 = P("Marry", F, 1980, MaryChildren_expected2)
    let Mirka_expected2 = P("Mirka", F, 1990, [])
    let MayChildren_expected2 = [
        P("Fred", M, 1970, []);
        P("Joan", F, 1975, []);
        Mirka_expected2]
    let May_expected2 = P("May", F, 1945, MayChildren_expected2)

    let JoeChildren_expected2 = [
        P("Stanley", M, 1975, []);
        Marry_expected2;
        P("Jane", F, 1985, [])]
    let Joe_expected2 = P("Joe", M, 1950, JoeChildren_expected2)
    let Paul_expected2 = P("Paul", M, 1955, [])
    let LarryChildren_expected2 = [May_expected2; Joe_expected2; Paul_expected2]
    let Larry_expected2 = P("Larry", M, 1920, LarryChildren_expected2)

    // e)
    let Marry_truncated = P("Marry", F, 1980, [])
    let May_truncated = P("May", F, 1945, [])
    let JoeChildren_truncated = [
        P("Stanley", M, 1975, []);
        Marry_truncated;
        P("Jane", F, 1985, [])]
    let Joe_truncated = P("Joe", M, 1950, JoeChildren_truncated)
    let Paul_truncated = P("Paul", M, 1955, [])
    let LarryChildren_truncated = [May_truncated; Joe_truncated; Paul_truncated]
    let Larry_truncated = P("Larry", M, 1920, LarryChildren_truncated)


    // 3. Unit tests

    let isWF_true_UnitTest = 
        let result = isWF Larry
        printTestResult "isWF_true_UnitTest" (result = true)

    let isWF_false_1_UnitTest = 
        let result = isWF Charles
        printTestResult "isWF_false_1_UnitTest" (result = false)

    let isWF_true_fixed_false_1_UnitTest = 
        let result = isWF Charles2
        printTestResult "isWF_true_fixed_false_1_UnitTest" (result = true)

    let isWF_false_2_UnitTest = 
        let result = isWF Charles3
        printTestResult "isWF_false_2_UnitTest" (result = false)

    let makePerson_UnitTest = 
        let result = 
            match makePerson("Leopold", M, 1940) with
            | P(name, sex, year, childr) when 
                name = "Leopold"  && sex = M && year = 1940 && childr = [] 
                -> true
            | _ -> false
        printTestResult "makePerson_UnitTest" (result = true)

    let insertChildOf_None_UnitTest = 
        let old_person = makePerson("Leopold", M, 1940)
        let result = insertChildOf "Paul" old_person Larry
        printTestResult "insertChildOf_None_UnitTest" (result = None)

    let insertChildOf_Some_UnitTest = 
        let young_person = makePerson("Mirka", F, 1990)
        let result = match insertChildOf "Paul" young_person Larry with
                     | Some(x) -> true
                     | None -> false
        printTestResult "insertChildOf_None_UnitTest" (result = true)

    let insertChildOf_Some_WithCompare_simple_UnitTest = 
        let young_person = makePerson("Mirka", F, 1990)
        let result = match insertChildOf "Paul" young_person Larry with
                     | Some(x) -> compareTrees(x, Larry_expected)
                     | None -> false
        printTestResult "insertChildOf_Some_WithCompare_simple_UnitTest" (result)

    let insert_UnitTest = 
        let temp = insert Mirka_expected MayChildren 
        let result = temp |> Seq.pairwise |> Seq.forall (fun (a, b) -> getYear a <= getYear b)
        printTestResult "insert_UnitTest" (result)

    let insertChildOf_Some_WithCompare_complex_Test = 
        let young_person = makePerson("Mirka", F, 1990)
        let temp = insertChildOf "May" young_person Larry
        let result = match temp with
                     | Some(x) -> compareTrees(x, Larry_expected2)
                     | None -> false
        printTestResult "insertChildOf_Some_WithCompare_complex_Test" (result)

    let insertChildOf_None_1_Test = 
        let young_person = makePerson("Mirka", F, 1920)
        let temp = insertChildOf "May" young_person Larry
        let result = match temp with
                     | Some(x) -> false
                     | None -> true
        printTestResult "insertChildOf_None_1_Test" (result)

    let insertChildOf_None_2_Test = 
        let young_person = makePerson("Mirka", F, 1990)
        let temp = insertChildOf "Karol" young_person Larry
        let result = match temp with
                     | Some(x) -> false
                     | None -> true
        printTestResult "insertChildOf_None_2_Test" (result)

    let find_UnitTest = 
        let expected = (F, 1980, ["Peter"; "Bob"; "Eve"])
        let result = match find "Marry" Larry with
                     | Some(x) -> x = expected
                     | None -> false
        printTestResult "find_UnitTest" (result)

    let find_None_UnitTest = 
        let result = match find "Karol" Larry with
                     | Some(x) -> false
                     | None -> true
        printTestResult "find_UnitTest" (result)

    let toString_UnitTest = 
        let result = toString 6 Larry
        let expected = "Larry Male 1920
      May Female 1945
            Fred Male 1970
            Joan Female 1975
      Joe Male 1950
            Stanley Male 1975
            Marry Female 1980
                  Peter Male 2005
                  Bob Male 2008
                  Eve Female 2010
            Jane Female 1985
      Paul Male 1955\n"
        printTestResult "find_UnitTest" (String.Compare(result, expected) = 0)       

    let truncate_UnitTest =
        let result = truncate Larry
        printTestResult "truncate_UnitTest" (compareTrees(result, Larry_truncated))
