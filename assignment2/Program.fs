// Learn more about F# at http://fsharp.org

open System
type T = | A of T list
         | B of bool
         | C of T * T
         
let a = A []
let b = B true
let c = C (A [B false; B true], B true)
let d = C (A [B true; C (A [B true],B true);B false], A [B true; C (A [B true],B true);B false])




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

let wellDefWS (pr:PartReg) (tr:TaskReg) (tsk, prts) =
    Map.exists (fun t _ -> t = tsk) tr &&
    List.forall (fun (p, _) -> Map.exists (fun prt _ -> prt = p) pr) prts &&
    List.forall (fun (_, c) -> c > 0) prts

let wellDefAL (pr:PartReg) (tr:TaskReg) (al:AssemblyLine) =
    List.forall (wellDefWS pr tr) al
    
let rec longestDuration (al, tr) =
    match al with
    | []         -> 0
    | (t,_)::als -> match Map.tryFind t tr with
                    | None -> longestDuration (als, tr)
                    | Some (d, _) -> let temp = longestDuration (als, tr)
                                     if d > temp then d else temp
    
let rec partCostPS pr = function
    | []         -> 0
    | (p, c)::ps -> c * (Map.find p pr) + partCostPS pr ps

let rec partCostAL pr = function
    | []             -> 0
    | (_, prts)::als -> partCostPS pr prts + partCostAL pr als

let rec prodDurCost tr = function
    | []         -> (0,0)
    | (t,_)::als -> let (d1, c1) = prodDurCost tr als
                    let (d2, c2) = Map.find t tr
                    (d1+d2, c1+c2)

let toStock al =
    List.foldBack (fun (_, ps) tail ->
                   List.foldBack (fun (p, c) t ->
                                  match Map.tryFind p t with
                                  | None -> Map.add p c t
                                  | Some (oc) -> Map.add p (c+oc) t
                                 ) ps tail
                  ) al Map.empty
    

// tests
wellDefAL preg1 treg1 al1
longestDuration (al1, treg1)
partCostAL preg1 al1
prodDurCost treg1 al1
toStock al1

[<EntryPoint>]
let main argv =
    printfn "Hello World from F#!"
    0 // return an integer exit code
