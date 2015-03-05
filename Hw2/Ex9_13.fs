// Антропов Игорь Михайлович 171гр 06.03.2015
// Реализация первой части (не касающийся деревьев)
//   второго задания и unit test'ов для нее.
// Ожидаемое время выполнения 2 часа.
// Реальное время выполнения 2.5 часа.


let rec fold f value list =
    match list with
    | []        -> value
    | x :: list -> fold f (f value x) list



[<EntryPoint>]
let main argv = 
    let temp = List.iter //val temp : (('a -> unit) -> 'a list -> unit)

    let list = [1; 2; 3; 4; 5; 0] //Произвольный список
    printfn "%A" list

    let reverseList = fold (fun x y -> y :: x) [] list //Переворот произвольного списка
    printfn "%A" reverseList

    let filterList = List.fold ( fun x y -> if (y > 3) then  y :: x else x) [] list // Фильтр по условию 
    printfn "%A" filterList                                                         // в данном случае выбрал элементы >3

    let mapList = fold (fun x y -> y+5 :: x) [] (fold (fun x y -> y :: x) [] list) // Применение ф-ии ко всем эл-ам списка
    printfn "%A" mapList                                                           // Тут прибавил ко всем 5

    let x = 5
    let Horner list = List.fold (fun an1 an2 -> an1*x + an2) 0 list // Подсчет значения многочлена в определенной точке
    printfn "%d" (Horner list)                                      // Точка = 5. Коэфициенты все тот же список.
    0 
