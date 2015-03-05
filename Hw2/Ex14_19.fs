// Антропов Игорь Михайлович 171гр 06.03.2015
// Реализация второй части (касающийся деревьев)
//   второго задания и unit test'ов для нее.
// Ожидаемое время выполнения 2 часа.
// Реальное время выполнения 2.5 часа.

type tree<'A> = Nil | Node of tree<'A> * 'A * tree<'A> // Задание полиморфного дерева

let rec poliMap f tree =      // Применение какой либо финкции к дереву
  match tree with
  | Nil -> Nil
  | Node (l, c, r) -> Node (poliMap f l, f c , poliMap f r)

let rec poliFold f a tree =  // Свертка дерева 
  match tree with
  | Nil   -> a
  | Node (l, c, r) -> 
      let c = f a c 
      poliFold f (poliFold f c l) r 


let sumTree tree = poliFold (+) 0 tree // Ф-ия суммы значений дерева 
                                       // Будет работать если элементы дерева можно сладывать


let minOpt a b =  // ф-ия сравнения 2ух Optiona чисел
  match a with
  | None      -> Some(b)
  | Some(a)   -> Some(min a b)
    
let findMin tree = poliFold minOpt None tree // Поиск наименьшего значения в дереве

let rec insert tree value = // Реализация вставки(брал из предыдущего задания. 
  match tree with           // Будет необходима для копирования 
  | Nil -> Node (Nil, value, Nil)
  | Node (l, c, r) -> 
    if c = value then Node (l, value, r) 
      elif c > value then Node (insert l value, c, r) 
        else Node (l, c, insert r value)

let copeTree tree = poliFold insert Nil tree // Копирование дерева 

[<EntryPoint>]
let main argv = 
    let test = (Node (Node (Nil, 3, Node (Nil, 5, Nil)), 8, Node (Node (Nil, 9, Nil), 10, Node (Nil, 12, Nil)))) //Универсальный тест
    printfn "%A" test
    printfn "%A" (sumTree test) // Пример поиска суммы всех значений узлов
    printfn "%A" (poliMap (fun x  -> x+1) test) // Пример инкремента для каждого узла
    printfn "%A" (findMin test) // Пример поиска минимального элемента
    printfn "%A" (copeTree test) // Пример полной копия дерева
    0 
