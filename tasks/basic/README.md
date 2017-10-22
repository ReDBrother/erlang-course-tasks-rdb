#### 1.5. Lists/binary comprehensions

 - Напишите LC, который выдаст декартово умножение двух списков.
 
 `[{X, Y} || X <- List1, Y <- List2].`
 
 - Имеется список вида - [[1,2,3], [4,5,6], [7,8,9]]. С помощью какого LC мы
   можем получить плоский список?
 
 `[Elem || List <- Lists, Elem <- List].`
 
 - Имеется список словарей вида:
```erlang
Dicts = [
    #{
        tags => [awesome, erlang]
    },
    #{
        tags => [simple_tag]
    },
    #{
        tags => [just_atom, 'I am ok']
    }
]
```

Какой list comprehension выдаст список вида: `[awesome, erlang, simple_tag,
just_atom, 'I am ok']`?.

`[Tag || Dict <- Dicts, Tag <- maps:get(tags, Dict)].`

 - Имеется следующий список:

```erlang
MixedList = [
    john,
    doe,
    {age, 19},
    {height, 182},
    {weight, 72},
    london,
    britain
].
```
 - С помощью какого LC можно получить список вида? (см. ниже):
```erlang
 [
    {age, 19},
    {height, 182},
    {weight, 72}
 ]
```

`[{X, Y} || {X, Y} <- MixedList].`

- Какой LC выдаст список, в котором только атомы `[john, doe, london, britain]`?

`[X || X <- MixedList, is_atom(X)].`

 - Имеется список объектов, представляющих прямоугольники на плоскости,
   следующего вида:
```erlang
Shapes = [
    {{0, 0}, {10, 10}},
    {{0, 1}, {2, 30}},
    {{30, 31}, {40, 41}},
    {{32, 56}, {5, 9}}
]
```

Написать LC, результатом которого будет список прямоугольников с площадью меньше
N.

`[Shape || {{X1, Y1}, {X2, Y2}} = Shape <- Shapes, abs(X1 - X2) * abs(Y1 - Y2) < N].`
 - Написать binary comprehension, который сериализует список прямоугольников в
   бинарное представление.

`[<<X1:32/integer, Y1:32/integer, X2:32/integer, Y2:32/integer>> || {{X1, Y1}, {X2, Y2}} <- Shapes].`

 - Напишите list comprehension, распакует бинарную строку в список,
   эквивалентный списку Shapes.

`[{{X1, Y1}, {X2, Y2}} || <<X1:32/integer, Y1:32/integer, X2:32/integer, Y2:32/integer>> <- Bins].`

#### 1.6. Булевые операции
[bool.erl](https://github.com/ReDBrother/erlang-course-tasks-rdb/blob/master/tasks/basic/src/bool.erl)
 - Напишите модуль bool.erl и определите в нём логические операции
   - `b_not/1`
   - `b_and/2`
   - `b_or/2`
   - `b_xor/2`

на атомах `true` и `false`. При определении функций пользуйтесь сопоставлением с
образцом, а не встроенными функциями and, or и not. Ниже приведены примеры
использования модуля в интерпретаторе:

```erlang
1> c(bool).
{ok,bool}
2> bool:b_not(true).
false
3> bool:b_and(true, true).
true
4> bool:b_and(true, false).
false
5> bool:b_or(true, false).
true
6> bool:b_or(false, false).
false
7> bool:b_or(false, true). 
true
8> bool:b_not(bool:b_or(false, true)).
false
9> bool:b_xor(true, false).
true
10> bool:b_xor(true, true). 
false
```

#### 1.7.  Работа со списками.
[list.erl](https://github.com/ReDBrother/erlang-course-tasks-rdb/blob/master/tasks/basic/src/list.erl)
 - Напишите функцию `create/1`, которая на вход принимает число N и возвращает
   список вида `[1, 2,..., N -1, N]`.
 - Напишите функцию, которая также принимает число N, но возвращает список вида
   `[N, N-1, ..., 2, 1]`.
 - Напишите функцию, которая распечатывает все числа от 1 до N.
 - Напишите функцию, которая распечатывает все нечётные числа от 1 до N.
 - Напишите функцию, которая принимает на вход список целых чисел и одно целое
   число, а возвращает список всех элементов списка, которые меньше либо равны
   числу, переданному вторым аргументом. Пример: `filter([1, 2, 3, 4, 5], 3) =>
   [1,2,3].`
 - Напишите функцию, которая переворачивает список. Пример: `reverse([1,2,3]) =>
   [3,2,1].`
 - Напишите функцию, которая преобразует список списков в один список, соединяя
   все списки-элементы. Пример: `concatenate([[1,2,3], [], [4, five]]) =>
   [1,2,3,4,five].`. Подсказка: вам придётся написать вспомогательную функцию и
   выполнить объединение списка в несколько действий. Функции должны быть
   написанф без использования list comprehension и `++`.
 - Напишите функцию, которая по списку вложенных списков строит линейный список.
   Пример: `flatten([[1, [2 , [3] , [] ], [[[4]]], [5,6]]]) => [1,2,3,4,5,6].`.
   Подсказка: воспользуйтесь функцией concatenate.
[dna.erl](https://github.com/ReDBrother/erlang-course-tasks-rdb/blob/master/tasks/basic/src/dna.erl)
- Напишите функцию, которая принимает на вход последовательность нуклеотидов
   ДНК и выдает комплементарную ей цепочку нуклеотидов РНК. Траскрипция
   происходит путем замены нуклеотидов в исходной цепочке ДНК на парные им
   нуклеотиды, входящие в цепочку РНК. ДНК содержит четыре нуклеотида: `A` -
   аденин, `С` - цитозин, `G` - гуанин, `T` - тимин. В РНК содержатся следующие
   нуклеотиды: `A` - аденин, `С` - цитозин, `G` - гуанин, `U` - урацил. Словарь
   замены:
   - `G` -> `C`
   - `C` -> `G`
   - `T` -> `A`
   - `A` -> `U`
   Функция должна принимать либо список вида `"AAGGUU"` либо список вида `[a, a,
   g, g, u, u]`.
 - Напишите функцию, которая из заданной цепочки РНК/ДНК вырезает заданную
   последовательность из трех нуклеотидов. Пример: `cut_rdna("AAGGTT", "AGG")
   => "ATT"`.

#### 1.8. Работа с JSON объектами
[json.erl](https://github.com/ReDBrother/erlang-course-tasks-rdb/blob/master/tasks/basic/src/json.erl)
Реализуйте модуль для работы с JSON объектами со следующей спецификацией.
Используйте ``map()`` для представления JsonObj.

```erlang
Key = string(),
KeySpec = string(),
BasicValue = string() | boolean() | integer() | float()
ValueSpec = BasicValue | [BasicValue] | {Key, ValueSpec} | [{Key, ValueSpec}]

json:new([{Key, ValueSpec}]) -> JsonObj.
json:read(KeySpec, JsonObj) -> {ok, ValueSpec} | {error, not_found}
json:write(KeySpec. ValueSpec, JsonObj) -> JsonObj | {error, not_found}
```
