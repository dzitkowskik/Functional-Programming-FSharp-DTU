﻿#light

module Assignment_2 =

(* ASSIGNMENT 2
A company consists of departments with sub-departments, which again can have
sub-departments, and so on. (The company can also be considered as a department.)

1. Assume that each department has a name and a (possibly empty) list of sub-departments.
Declare an F# type Department.

2. Extend this type so that each department has its own gross income.

3. Declare a function to extract a list of pairs (department name, gross income), 
for all departments.

4. Declare a function to extract the total income for a given department by adding up its gross
income, including the income of its sub-departments.

5. Declare a function to extract a list of pairs (department name, total income) 
for all departments.

6. Declare a function format of type Department->string, which can be used to get a
textual form of a department such that names of sub-departments will occur suitably indented 
(e.g., with four spaces) on separate lines. (Use printf to print out the result.
Do not use printf in the declaration of format.)
*)


// 1) & 2)

    type Department = {
        Name : string;
        SubDepartments : Department list;
        GrossIncome : float;
        }

// 3)

    // Department -> string * float
    let toInfo dep = (dep.Name, dep.GrossIncome)

    // Department -> (string * float) list
    let rec extractDepartments dep =
        if dep.SubDepartments.IsEmpty then [toInfo dep]
        else toInfo dep :: List.concat (List.map extractDepartments dep.SubDepartments)

    // Department -> (string * float) list
    let rec extractDepartments2 company = 
        let rec loop deps acc =
            match deps with
            | head :: tail -> loop tail (extractDepartments2 head @ acc)
            | [] -> acc
        loop company.SubDepartments [toInfo company]

// 4)

    // Department -> float
    let rec getGrossIncomeSum dep = 
        if dep.SubDepartments.IsEmpty then dep.GrossIncome
        else (List.sumBy getGrossIncomeSum dep.SubDepartments) + dep.GrossIncome

    // Department -> float
    let rec getGrossIncomeSum2 dep = 
        let rec loop deps gross =
            match deps with
            | head :: tail -> loop tail (gross + getGrossIncomeSum2 head)
            | [] -> gross
        loop dep.SubDepartments dep.GrossIncome

// 5)

    // Department -> (string * float) list
    let rec sumUpDepartments dep =  
        let subResults = List.concat (List.map sumUpDepartments dep.SubDepartments)
        let subDeps = 
            subResults
            |> List.filter (fun x -> List.exists (fun y -> fst x = y.Name) dep.SubDepartments)
        (dep.Name, dep.GrossIncome + List.sumBy (fun x -> snd x) subDeps) :: subResults

    // Department -> (string * float) list
    let rec sumUpDepartments2 dep = 
        let rec loop deps acc =
            match deps with
            | head :: tail -> loop tail (sumUpDepartments2 head @ acc)
            | [] -> acc
        loop dep.SubDepartments [(dep.Name, getGrossIncomeSum2 dep)]

// 6)

    // Department -> string
    let printOutCompany company = 
        let tab = "\t"
        let rec printOut dep ident =
            let text = ident + dep.Name + "\n"
            List.foldBack (fun elem acc -> acc + printOut elem (ident+tab)) dep.SubDepartments text
        printOut company ""

// OTHER)

    // Department -> bool
    let printOutCompanyToStdOut company = 
        let tab = "\t"
        let rec printOut dep ident =
            printf "%s%s\n" ident dep.Name
            List.forall (fun x -> printOut x (ident+tab)) dep.SubDepartments
        printOut company ""

//#if DEBUG

    //////////////////////////////
    //           TESTS          //
    //////////////////////////////

    // 1. Helper functions

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

    let Dep6 = { Name = "Sixth dep"; GrossIncome = 111.0; SubDepartments = []; }
    let Dep5 = { Name = "Fifth dep"; GrossIncome = 200.0; SubDepartments = []; }
    let Dep4 = { Name = "Fourth dep"; GrossIncome = 100.0; SubDepartments = []; }
    let Dep3 = { Name = "Third dep"; GrossIncome = 600.0; SubDepartments = [Dep5]; }
    let Dep2 = { Name = "Second dep"; GrossIncome = 500.0; SubDepartments = [Dep4]; }
    let Dep1 = { Name = "First dep"; GrossIncome = 1000.0; SubDepartments = [Dep2; Dep3]; }
    let CompanyA = { Name = "Asseco Sp.Z.O.O"; GrossIncome = 2500.0; SubDepartments = [Dep1]; }
    let CompanyB = Dep1
    let CompanyC = { Name = "Google Inc."; GrossIncome = 10000.0; 
                     SubDepartments = [Dep6;Dep5;Dep2] }

    // 3. Unit tests

    let extractDepartments_UnitTest = 
        let result = extractDepartments CompanyA
        let expected = [toInfo CompanyA; toInfo Dep1; toInfo Dep2;
                        toInfo Dep3; toInfo Dep4; toInfo Dep5]
        printTestResult "extractDepartments_UnitTest" (compareLists result expected)

    let extractDepartments_noSubDep_UnitTest = 
        let result = extractDepartments Dep6
        let expected = [(Dep6.Name, Dep6.GrossIncome)]
        printTestResult "extractDepartments_noSubDep_UnitTest" (compareLists result expected)

    let extractDepartments2_UnitTest = 
        let result = extractDepartments2 CompanyA
        let expected = [toInfo CompanyA; toInfo Dep1; toInfo Dep2;
                        toInfo Dep3; toInfo Dep4; toInfo Dep5]
        printTestResult "extractDepartments2_UnitTest" (compareLists result expected)

    let extractDepartments2_noSubDep_UnitTest = 
        let result = extractDepartments2 Dep6
        let expected = [(Dep6.Name, Dep6.GrossIncome)]
        printTestResult "extractDepartments2_noSubDep_UnitTest" (compareLists result expected)

    let getGrossIncomeSum_UnitTest =
        let result = getGrossIncomeSum CompanyA
        let expected = 2500.0 + 1000.0 + 500.0 + 600.0 + 100.0 + 200.0
        printTestResult "getGrossIncomeSum_UnitTest" (result = expected)

    let getGrossIncomeSum_noSubDep_UnitTest =
        let result = getGrossIncomeSum Dep4
        let expected = 100.0
        printTestResult "getGrossIncomeSum_noSubDep_UnitTest" (result = expected)

    let getGrossIncomeSum2_UnitTest =
        let result = getGrossIncomeSum2 CompanyA
        let expected = 2500.0 + 1000.0 + 500.0 + 600.0 + 100.0 + 200.0
        printTestResult "getGrossIncomeSum2_UnitTest" (result = expected)

    let getGrossIncomeSum2_noSubDep_UnitTest =
        let result = getGrossIncomeSum2 Dep4
        let expected = 100.0
        printTestResult "getGrossIncomeSum2_noSubDep_UnitTest" (result = expected)

    let sumUpDepartments_noSubDep_UnitTest =
        let result = sumUpDepartments Dep5
        let expected = [(Dep5.Name, Dep5.GrossIncome)]
        printTestResult "sumUpDepartments_noSubDep_UnitTest" (compareLists result expected)

    let sumUpDepartments_CompanyA_UnitTest =
        let result = sumUpDepartments CompanyA
        let expected = [(Dep5.Name, Dep5.GrossIncome);
                        (Dep4.Name, Dep4.GrossIncome);
                        (Dep3.Name, Dep3.GrossIncome+Dep5.GrossIncome);
                        (Dep2.Name, Dep2.GrossIncome+Dep4.GrossIncome);
                        (Dep1.Name, 2400.0);
                        (CompanyA.Name, 4900.0)]
        printTestResult "sumUpDepartments_noSubDep_UnitTest" (compareLists result expected)

    let sumUpDepartments2_noSubDep_UnitTest =
        let result = sumUpDepartments2 Dep5
        let expected = [(Dep5.Name, Dep5.GrossIncome)]
        printTestResult "sumUpDepartments2_noSubDep_UnitTest" (compareLists result expected)

    let sumUpDepartments2_CompanyA_UnitTest =
        let result = sumUpDepartments2 CompanyA
        let expected = [(Dep5.Name, Dep5.GrossIncome);
                        (Dep4.Name, Dep4.GrossIncome);
                        (Dep3.Name, Dep3.GrossIncome+Dep5.GrossIncome);
                        (Dep2.Name, Dep2.GrossIncome+Dep4.GrossIncome);
                        (Dep1.Name, 2400.0);
                        (CompanyA.Name, 4900.0)]
        printTestResult "sumUpDepartments2_noSubDep_UnitTest" (compareLists result expected)


    let printOutCompany_UnitTest =
        let result = printOutCompany CompanyA
        let expected = "Asseco Sp.Z.O.O\n\tFirst dep\n\t\tThird dep\n\t\t\tFifth dep\n\t\tSecond dep\n\t\t\tFourth dep\n"
        printTestResult "printOutCompany_UnitTest" (result = expected)
    
    printOutCompanyToStdOut CompanyA |> ignore

//#endif