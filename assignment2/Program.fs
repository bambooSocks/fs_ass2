// Learn more about F# at http://fsharp.org

open System
//type T = | A of T list
//         | B of bool
//         | C of T * T
//         
//let a = A []
//let b = B true
//let c = C (A [B false; B true], B true)
//let d = C (A [B true; C (A [B true],B true);B false], A [B true; C (A [B true],B true);B false])




type Part = string
type Task = string
type Cost = int (* can be assumed to be positive *)
type Duration = int (* can be assumed to be positive *)
type PartReg = Map<Part, Cost>
type TaskReg = Map<Task, Duration*Cost>
(* Part and task registers for balance bikes *)
let preg1 = Map.ofList [("wheel",50); ("saddle",10); ("handlebars",75); ("frame",100); ("screw bolt",5); ("nut",3)];;
let treg1 = Map.ofList [("addWheels",(10,2)); ("addSaddle",(5,2)); ("addHandlebars",(6,1))]

type WorkStation = Task * (Part*int) list
type AssemblyLine = WorkStation list
let ws1 = ("addWheels",[("wheel",2);("frame",1);("screw bolt",2);("nut",2)])
let ws2 = ("addSaddle",[("saddle",1);("screw bolt",1);("nut",1)])
let ws3 = ("addHandlebars",[("handlebars",1);("screw bolt",1);("nut",1)])
let al1 = [ws1; ws2; ws3];;

type Stock = Map<Part, int>

let wellDefWS pr tr (tsk, prts) =
    Map.exists (fun t _ -> t = tsk) tr &&
    List.forall (fun (p, _) -> Map.exists (fun prt _ -> prt = p) pr) prts &&
    List.forall (fun (_, c) -> c > 0) prts

let wellDefAL (pr:PartReg) (tr:TaskReg) (al:AssemblyLine) =
    List.forall (fun ws -> wellDefWS pr tr ws) al
    
let longestDuration (al, treg) = 0
    
    
let partCostAL pr al = 0

let prodDurCost tr al = 0, 0

let toStock al = Stock

[<EntryPoint>]
let main argv =
    printfn "Hello World from F#!"
    0 // return an integer exit code
