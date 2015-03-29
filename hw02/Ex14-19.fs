// Антропов Игорь Михайлович 171гр 06.03.2015
// Реализация второй части (касающийся деревьев)
//   второго задания и unit test'ов для нее.
// Ожидаемое время выполнения 2 часа.
// Реальное время выполнения 2.5 часа.


type Tree<'A> = Nil | Node of Tree<'A> * 'A * Tree<'A> // Задание полиморфного дерева

let rec Map f tree =      // Применение какой либо финкции к дереву
  match tree with
  | Nil -> Nil
  | Node (l, c, r) -> Node (Map f l, f c , Map f r)

let rec Fold f a tree =  // Свертка дерева 
  match tree with
  | Nil   -> a
  | Node (l, c, r) -> 
      let c = f a c 
      Fold f (Fold f c l) r 


let sumTree tree = Fold (+) 0 tree // Ф-ия суммы значений дерева 
                                       // Будет работать если элементы дерева можно сладывать


let minOpt a b =  // ф-ия сравнения 2ух Optiona чисел
  match a with
  | None      -> Some(b)
  | Some(a)   -> Some(min a b)
    
let findMin tree = Fold minOpt None tree // Поиск наименьшего значения в дереве

let rec insert tree value = // Реализация вставки
  match tree with           // Будет необходима для копирования 
  | Nil -> Node (Nil, value, Nil)
  | Node (l, c, r) -> 
    if c = value then Node (l, value, r) 
      elif c > value then Node (insert l value, c, r) 
        else Node (l, c, insert r value)

let copyTree tree = Fold insert Nil tree // Копирование дерева 

let rec printСLR l =
  match l with
  | Nil -> printf ""
  | Node (l, c, r) -> printf "%A " c; printСLR l; printСLR r

let rec printLRC l =
  match l with
  | Nil -> printf ""
  | Node (l, c, r) -> printLRC l; printLRC r; printf "%A " c

let rec printLCR l =
  match l with
  | Nil -> printf ""
  | Node (l, c, r) -> printLCR l; printf "%A " c; printLCR r


[<EntryPoint>]
let main argv = 
    let test = (Node (Node (Nil, 3, Node (Nil, 5, Nil)), 8, Node (Node (Nil, 9, Nil), 10, Node (Nil, 12, Nil)))) //Универсальный тест
    printf "Tree example: "; printf "%A" test
    printf "\nCLR : "; printСLR test
    printf "\nLRC : "; printLRC test
    printf "\nLCR : "; printLCR test

    printf "\n \nСумма значений излов дерева: ";  
    printfn "%A" (sumTree test) // Пример поиска суммы всех значений узлов

    printf "\nМинимльное значение дерева: "
    printf "%A" (findMin test) // Пример поиска минимального элемента
        
    printf "\n \nПример использования map добавляя к каждому узлу 3: "; 
    printf "\nCLR : "; printСLR (Map (fun x  -> x+3) test) 
    printf "\nLRC : "; printLRC (Map (fun x  -> x+3) test) 
    printf "\nLCR : "; printLCR (Map (fun x  -> x+3) test) 
    
    printf "\n \nСкопированное дерево:"
    let copiedtree = copyTree test
    printf "\nCLR : "; printСLR copiedtree
    printf "\nLRC : "; printLRC copiedtree
    printf "\nLCR : "; printLCR copiedtree
    0 
