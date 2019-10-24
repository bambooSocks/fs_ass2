// Second Assignemnt for course 02157 Functional programming
// Mihaela-Elena Nistor - s183190, Matej Majtan - s184457

// Task 1.1
//type T = | A of T list
//         | B of bool
//         | C of T * T
//         
//let a = A []
//let b = B true
//let c = C (A [B false; B true], B true)
//let d = C (A [B true; C (A [B true],B true);B false], A [B true; C (A [B true],B true);B false])
//
//


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

// Task 2.1
let wellDefWS (pr:PartReg) (tr:TaskReg) (tsk, prts) =
    Map.exists (fun t _ -> t = tsk) tr &&
    List.forall (fun (p, _) -> Map.exists (fun prt _ -> prt = p) pr) prts &&
    List.forall (fun (_, c) -> c > 0) prts

// Task 2.2
let wellDefAL (pr:PartReg) (tr:TaskReg) (al:AssemblyLine) =
    List.forall (wellDefWS pr tr) al

// Task 2.3
let longestDuration (al, tr) =
    List.foldBack (fun (t, _) als -> match Map.tryFind t tr with
                                     | None -> als
                                     | Some (d, _) -> List.max [d; als]
                  ) al 0

// Task 2.4
let partCostAL pr al =
    List.foldBack (fun (_, ps) als ->
                  (List.foldBack (fun (p, c) pts ->
                                  c * (Map.find p pr) + pts
                                 ) ps 0) + als
                  ) al 0



// Task 2.5
let prodDurCost tr al =
    List.foldBack (fun (t, _) als -> let (d1, c1) = Map.find t tr
                                     let (d2, c2) = als
                                     (d1+d2, c1+c2)
                  ) al (0,0)

// Task 2.6
let toStock al =
    List.foldBack (fun (_, ps) tail ->
                   List.foldBack (fun (p, c) t ->
                                  match Map.tryFind p t with
                                  | None -> Map.add p c t
                                  | Some (oc) -> Map.add p (c+oc) t
                                 ) ps tail
                  ) al Map.empty
    

// TESTS
wellDefWS preg1 treg1 ws1
wellDefAL preg1 treg1 al1
longestDuration (al1, treg1)
partCostAL preg1 al1
prodDurCost treg1 al1
toStock al1

