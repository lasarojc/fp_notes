Uma função é dita recursiva quando ela é **definida em termos de si mesma**.
A matemática é rica em exemplo de funções recursivas, também conhecidas como **recorrências**.
Alguns exemplos ...


Nós já mostramos algumas destas funções, sem chamar a atenção para o fato de ser recursiva.



```hs
maximum [] = error "lista vazia"  
maximum [h] = h
maximum (h:t) = max h (maximum t)
```

$maximum~[1,2,3] = max~1 \left( maximum~[2,3] = max~2 \left( maximum~[3] = 3 \right)        \right)$