// Second Assignemnt for course 02157 Functional programming
// Mihaela-Elena Nistor - s183190, Matej Majtan - s184457

type Part = string
type Task = string
type Cost = int (* can be assumed to be positive *)
type Duration = int (* can be assumed to be positive *)
type PartReg = Map<Part, Cost>
type TaskReg = Map<Task, Duration*Cost>
(* Part and task registers for balance bikes *)

type WorkStation = Task * (Part*int) list
type AssemblyLine = WorkStation list

type Stock = Map<Part, int>

// Task 2.1
let wellDefWS pr tr (tsk, prts) =
    Map.exists (fun t _ -> t = tsk) tr &&
    List.forall (fun (p, _) -> Map.exists (fun prt _ -> prt = p) pr) prts &&
    List.forall (fun (_, c) -> c > 0) prts

// Task 2.2
let wellDefAL pr tr al =
    List.forall (wellDefWS pr tr) al

// Task 2.3
// The type is AssemblyLine*TaskRegister -> int
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


let preg1 = Map.ofList [("wheel",50); ("saddle",10); ("handlebars",75); ("frame",100); ("screw bolt",5); ("nut",3)];;
let treg1 = Map.ofList [("addWheels",(10,2)); ("addSaddle",(5,2)); ("addHandlebars",(6,1))]    
let ws1 = ("addWheels",[("wheel",2);("frame",1);("screw bolt",2);("nut",2)])
let ws2 = ("addSaddle",[("saddle",1);("screw bolt",1);("nut",1)])
let ws3 = ("addHandlebars",[("handlebars",1);("screw bolt",1);("nut",1)])
let ws4 = ("addWheels",[("wheel",2);("frame",1);("screw bolt",2);("nut",-2)])
let ws5 = ("addWheels",[("wheel",2);("frame",1);("screw bolt",2);("nut",2);("chain",3)])
let ws6 = ("addChain",[("wheel",2);("frame",1);("screw bolt",2);("nut",2)])
let ws7 = ("addChain",[("handlebars",2);("frame",1)])
let al1 = [ws1; ws2; ws3];;
let al2 = [ws1; ws2; ws4];;
let al3 = [ws1; ws2; ws6];;
let al4 = [ws1; ws2; ws5];;
let al5 = [ws6];;
let al6 = [ws7];;
let al7= [ws6;ws7]

// TESTS

// Task 2.1 wellDefWS

// checks if all the conditions of the function are met. Expected result : true
let test1 = (wellDefWS preg1 treg1 ws1)
// checks if all the conditions of the function are met. Expected result : false 
let test2 = (wellDefWS Map.empty Map.empty ws4)
//checks if the first condition of the function works. Expected result : false
let test3 =  (wellDefWS preg1 treg1 ws6)
//checks if the second condition of the function works. Expected result :false
let test4 =  (wellDefWS preg1 treg1 ws5)
// checks if the third condition of the function works. Expected result : false
let test5 = (wellDefWS preg1 treg1 ws4)

// Task 2.2 wellDefAL

//checks if the condition of the function is met.Expected result: true
let test6 = wellDefAL preg1 treg1 al1
//checks if the condition of the function is met.Expected result: false
let test7 =  wellDefAL preg1 treg1 al2
// checks if the condition of the function is met.Expected result : false
let test8 = wellDefAL preg1 treg1 al3
// checks if the condition of the function is met.Expected result : false
let test9 = wellDefAL preg1 treg1 al4

// Task 2.3 longestDuration

//return the longest duration in al1 which is 10 for task "addWheels"
let test10 = longestDuration (al1,treg1)
//checks if task from al5 can be found in treg1 . The task is not in treg and therefore returns 0
let test11= longestDuration (al5,treg1)

// Task 2.4 partCostAL

//he accumulated cost of all parts of a balanced bike is 317 -
//the cost of one frame, two wheels, one saddle, handlebars, 4 nuts and 4 screw bolts.
let test12 = partCostAL preg1 al1
//  1frame (100) + 2 handleBars (2*75) = 250
let test13 =  partCostAL preg1 al6

// Task 2.5 prodDurCost

// returns a tuple of total duration of tasks in al1 which is (10("addWheels") + 5 ("addSaddle") + 6("addHandlebars"), 2("addWheels")+2("addSaddle")+1("addHandlebars")) =(21,5)
let test14 = prodDurCost treg1 al1
//repeats the same procedure as in test14 with the result (25,6)
let test15 = prodDurCost treg1 al2

// Task 2.6 toStock

//the function computes the stock needed for 1 single product which consists in this case of 2 handlebars and 1 frame 
let test16 = toStock al6
////the function computes the stock needed for 1 single product which consists of 2 wheels, 2 frames, 2 screw bolts , 2 nuts and 2 handlebars
let test17 = toStock al7
