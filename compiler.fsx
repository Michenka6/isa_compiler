open System.IO

type Op =
    | ADD of int * int * int
    | SUB of int * int * int
    | OR of int * int * int
    | AND of int * int * int
    | NOT of int * int
    | LI of int * int
    | LD of int * int
    | SD of int * int
    | JR of int
    | JEQ of int * int * int
    | JLT of int * int * int
    | NOP
    | END;;

type Memory = Map<int, int>;;

type Program = Map<int, Op>;;

let readLines (filePath:string) = seq {
    use sr = new StreamReader (filePath)
    while not sr.EndOfStream do
        yield sr.ReadLine ()
};;

let rec splitInstruction = function
    | "" -> []
    | str -> if (str |> Seq.exists (fun x -> x = ' '))
                then str[0..((str |> Seq.findIndex (fun x -> x = ' ')) - 1)] :: splitInstruction str[((str |> Seq.findIndex (fun x -> x = ' ')) + 1)..]
                else [str];;

let rec listToOp = function
    | [] -> []
    | (x :: xs) :: tail -> match x with
                            | "ADD" -> ADD (int (List.item 0 xs), int (List.item 1 xs), int (List.item 2 xs)) :: listToOp tail
                            | "SUB" -> SUB (int (List.item 0 xs), int (List.item 1 xs), int (List.item 2 xs)) :: listToOp tail
                            | "OR" -> OR (int (List.item 0 xs), int (List.item 1 xs), int (List.item 2 xs)) :: listToOp tail
                            | "AND" -> AND (int (List.item 0 xs), int (List.item 1 xs), int (List.item 2 xs)) :: listToOp tail
                            | "NOT" -> NOT (int (List.item 0 xs), int (List.item 1 xs)) :: listToOp tail
                            | "LI" -> LI (int (List.item 0 xs), int (List.item 1 xs)) :: listToOp tail
                            | "LD" -> LD (int (List.item 0 xs), int (List.item 1 xs)) :: listToOp tail
                            | "SD" -> SD (int (List.item 0 xs), int (List.item 1 xs)) :: listToOp tail
                            | "JR" -> JR (int (List.item 0 xs)) :: listToOp tail
                            | "JEQ" -> JEQ (int (List.item 0 xs), int (List.item 1 xs), int (List.item 2 xs)) :: listToOp tail
                            | "JLT" -> JLT (int(List.item 0 xs), int (List.item 1 xs), int (List.item 2 xs)) :: listToOp tail
                            | "NOP" -> NOP :: listToOp tail
                            | "END" -> END :: listToOp tail
                            | _ -> failwith "Not an assembly command"
    | _ -> [];;

let rec commandsToProgram n (map:Program) = function
    | [] -> map
    | head :: tail -> commandsToProgram (n+1) (map |> Map.add n head) tail;;

let intToInt x = if x > 0 then 1 else 0;;

let boolToInt = function
    | true -> 1
    | _ -> 0;;

let evalStep (memory: Memory) (program: Program) currentNumber =
    match (program |> Map.find currentNumber) with
        | ADD (reg, x, y) -> ((memory |> Map.add reg ((memory |> Map.find x) + (memory |> Map.find y))), (currentNumber + 1))
        | SUB (reg, x, y) -> ((memory |> Map.add reg ((memory |> Map.find x) + (memory |> Map.find y))), (currentNumber + 1))
        | OR (reg, x, y) -> ((memory |> Map.add reg (boolToInt (((memory |> Map.find x) > 0) || ((memory |> Map.find y) > 0)))), (currentNumber + 1))
        | AND (reg, x, y) -> ((memory |> Map.add reg (boolToInt (((memory |> Map.find x) > 0) && ((memory |> Map.find y) > 0)))), (currentNumber + 1))
        | NOT (reg, x) -> ((memory |> Map.add reg (boolToInt ((memory |> Map.find x) < 0))), (currentNumber + 1))
        | LI (reg, x) -> ((memory |> Map.add reg x), (currentNumber + 1))
        | LD (reg, x) -> ((memory |> Map.add reg (memory |> Map.find x)), (currentNumber + 1))
        | SD (reg, x) -> ((memory |> Map.add x (memory |> Map.find reg)), (currentNumber + 1))
        | JR (reg) -> (memory, (memory |> Map.find reg))
        | JEQ (reg, x, y) -> if (memory |> Map.find x) = (memory |> Map.find y)
                                then (memory, (memory |> Map.find reg))
                                else (memory, (currentNumber + 1))
        | JLT (reg, x, y) -> if (memory |> Map.find x) < (memory |> Map.find y)
                                then (memory, (memory |> Map.find reg))
                                else (memory, (currentNumber + 1))
        | NOP -> (memory, (currentNumber + 1))
        | END -> (memory, 1000);;

let rec evalProgram (program: Program) ((memory: Memory), currentNumber) = 
    if currentNumber >= (program |> Map.count)
        then memory
        else evalProgram program (evalStep memory program currentNumber);;



let input = readLines "/Users/amiralimov/Languages/FSharp/asm_compiler/input.txt";;

let afterSplit = input |> Seq.map splitInstruction |> Seq.toList;;

let afterTrans = listToOp afterSplit;;

let memory: Memory = Map.empty |> Map.add 0 0;;

let program = afterTrans |> commandsToProgram 0 Map.empty;;

let numberOfCommands = program |> Map.count;;

evalProgram program (memory, 0);;