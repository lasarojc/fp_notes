# Listas
Anteriormente nós discutimos tuplas como uma forma de agregar múltiplas informações em um único valor.
Por exemplo, definimos o tipo `Data` como sendo a agregação de um dia, um mês e um ano.

```hs
type Data :: (Int, Int Int)





###### Recursão e Listas

```hs
maximum [] = error "lista vazia"  
maximum [h] = h
maximum (h:t) = max h (maximum t)
```

$maximum~[1,2,3] = max~1 \left( maximum~[2,3] = max~2 \left( maximum~[3] = 3 \right)        \right)$