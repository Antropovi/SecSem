// Антропов Игорь Михайлович 171гр 06.03.2015
// Реализация первой части (не касающийся деревьев)
//   второго задания и unit test'ов для нее.
// Ожидаемое время выполнения 2 часа.
// Реальное время выполнения 2.5 часа.




let reverseList list = List.fold (fun x y -> y :: x) [] list

let filterList list f = List.foldBack  (fun x y-> if f x  then  x :: y else y) list []

let mapList list f = List.foldBack (fun x y ->  f x ::y) list []

let Horner list x = List.fold (fun an1 an2 -> an1*x+an2) 0 list


[<EntryPoint>]
let main argv = 
    let temp = List.iter //val temp : (('a -> unit) -> 'a list -> unit)
    printf "List.iter //val temp : (('a -> unit) -> 'a list -> unit)"
    let list = [1; 2; 3; 4; 5; 0] //Произвольный список
    printf "\n \nПример списка: "; printfn "%A" list

    printf "Перевернутый список: "; printfn "%A" (reverseList list) 

    printf "Только элементы больше 3: "; printfn "%A" (filterList list (fun x -> x>3))

    printf "Добавим к каждому элементу 5: "; printfn "%A" (mapList list (fun x-> 5+x))    

    let x = 2
    printfn "Примение метод Горнера к многочлену с коэф. %A в точке %i " list x ; 
    printfn "Ответ: %d" (Horner list 2)          



    0 