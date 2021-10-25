# Entrada e Saída

Em programação dizemos que uma função tem **efeitos colaterais** quando lê ou modifica qualquer estado além do que lhe foi passado como parâmetro e suas variáveis locais.
Linguagens de funcionais puras não tem **efeitos colaterais** e é por isso que toda vez que uma função é invocada com os mesmos parâmetros, ela retorna o mesmo resultado, assim como 2+2 = 4, sempre.

O problema de ser uma linguagem funcional pura está em realizar Entrada e Saída (E/S, I/O), ou seja, interagir com o mundo exterior ao programa em execução, por exemplo lendo do teclado, manipulando arquivos, ou mesmo modificando o estado da memória de vídeo para desenhar ou escrever algo na tela, pois isso é um efeito colateral.
Como E/S é uma realidade dos programas de computador, as linguagens funcionais devem ou abdicar de sua pureza ou usar de artimanhas para realizá-las.

No caso de Haskell, E/S é encapsulada em Mônadas, um conceito no qual não nos aprofundaremos neste curso.


## Ações de E/S
E/S é executada por meio de uma **ação E/S**, uma função especial que se comunica com o mundo "exterior".
As seguintes funções são exemplos de ações que se comunicam com teclado e monitor.

| Ação | Descrição |
|------|-----------|
| `#!hs getChar` | Lê um caractere do teclado. |
| `#!hs putChar` | Escreve um caractere na tela. |
| `#!hs getLine` | Lê uma linha do teclado. |
| `#!hs putStr` |  Escreve uma string na tela. |
| `#!hs putStrLn` | Escreve uma string na tela e quebra a linha. |


## *binding*
Uma vez que tenha visto estas funções, é natural que queira escrever algo como `#!hs nome = getLine`, mas isso não terá o efeito que você espera.

```hs
Prelude> nome = getLine
Prelude> nome
lalala
"lalala"
Prelude> putStr nome

<interactive>:11:8: error:
    • Couldn't match type ‘IO String’ with ‘[Char]’
      Expected type: String
        Actual type: IO String
    • In the first argument of ‘putStr’, namely ‘nome’
      In the expression: putStr nome
      In an equation for ‘it’: it = putStr nome
```

O que aconteceu no exemplo acima foi que a ação `#!hs getLine` foi atribuída a `#!hs nome`, não uma string.
Para fazer o que você quer, isto é, executar uma ação e atribuir o seu resultado a uma variável, precisamos usar o operador `#!hs <-`.

```hs
Prelude> nome <- getLine
lala
Prelude> print nome
"lala"
```

Dizemos que o operador `#!hs <-` faz o *binding* (atrelamento) do valor à variável.

## `#!hs IO`

Se explorarmos o tipo das ações veremos que elas não tem retorno como esperaríamos, por exemplo, `#!hs getLine` tem tipo  `#!hs getLine :: IO String`.
O `#!hs IO` no tipo do resultado é um tipo algébrico que encapsula o resultado da ação, mas diferenciar a ação de outras funções que resultem em `#!hs String`.

Todas as ações retornam um `#!hs IO a`, onde `#!hs a` é uma variável de tipo, mesmo aquelas que não retornam nada de útil, como a ação `#!hs putChar`, que resulta em `#!hs putChar :: Char -> IO ()`, isto é, o encapsulamento da tupla vazia (também conhecida como *Unit*).

Mas por quê esta diferenciação? Para marcar funções **impuras** e separá-las das funções **puras**.
Este estigma de uma função impura serve para que o compilador Haskell determine quando a ação pode executar e quando não pode, sendo que uma função impura só pode executar dentro de outra função impura.
Isso quer dizer que eu não consigo fazer uma ação de E/S e retornar somente o resultado desta ação, como em 

```hs
lêNomePura :: String
lêNomePura = x <- getLine
```

Na verdade, este código nem compila, pois o que é o resultado da expressão à direita na equação? Na verdade, ela nem é uma expressão.

```hs
> :t x <- getLine

<no location info>: error: not an expression: ‘x <- getLine’
```

Uma outra tentativa seria a seguinte. Mas como vimos, este código não executa a ação, apenas dá outro nome para ação `#!hs getLine`.

```hs
lêNomeImpura :: IO String
lêNomeImpura = getLine
```

## `#!hs do`
O problema aqui é que você não está descrevendo como um cálculo acontece, mas dizendo ao processo que execute ações.
Isso está muito mais para o paradigma imperativo que para o funcional.
E de fato, para executar ações, você precisara usar o modo imperativo do Haskell, usando `#!hs do`.

```hs
lêNomeImpura' :: IO String
lêNomeImpura' = do
                    putStr "Digite o seu nome"
                    getLine
```

O modo imperativo essencialmente diz que as ações devem ser executadas na ordem em que foram definidas e que o resultado do bloco é o resultado da última ação do bloco, neste exemplo, `#!hs getLine`.

## `#!hs return`
Nem sempre o resultado será gerado pela última ação, e neste caso você terá que construir o resultado manualmente, como no código a seguir, usando um `#!hs return` no final.

```hs
lêNomeImpura' :: IO String
lêNomeImpura' = do
                    putStr "Digite o seu nome"
                    x <- getLine
                    putStr "Obrigado"
                    return ("O nome é : " ++ x)
```

O `#!hs return` pode causar confusão, pois ele não faz o que você imagina. O que ele faz é construir um valor do tipo `#!hs IO` com o valor passado, e que pode ser usado como resposta para o bloco.

```hs
> x = return 3
> x
3
> :t x
x :: (Monad m, Num a) => m a
```

## let ~in~
Frequentemente você precisará intercalar código puro com código não puro, como no exemplo **incorreto** a seguir.

```hs
import Data.Char

main = do
        putStrLn "Digite seu nome: "
        nome <- getLine
        nomeMaiúsculo = map toUpper nome
        nomeMinúsculo = map toLower nome
        putStrLn $ "Olá. Seu nome em letras maiúsculas é " ++ nomeMaiúsculo ++ " e em letras minúsculas é " ++ nomeMinúsculo
```

Este exemplo, embora se assemelhe ao que outras linguagens fazem, não é correto em Haskell pois o código puro não pode ser invocado de forma imperativa, pois códigos puros são definições de equações, não instruções de execução.
A forma apropriada para fazer as definições de `#!hs nomeMaiúsculo` e `#!hs nomeMinúsculo` de forma que sejam usáveis na invocação de `#!hs putStrLn` é via `#!hs let` e `#!hs in`; bom, neste caso, como o contexto do `#!hs let` é bem claro, o `#!hs in` é omitido.

```hs
import Data.Char

main = do
        putStrLn "Digite seu nome: "
        nome <- getLine
        let nomeMaiúsculo = map toUpper nome
            nomeMinúsculo = map toLower nome
        putStrLn $ "Olá. Seu nome em letras maiúsculas é " ++ nomeMaiúsculo ++ " e em letras minúsculas é " ++ nomeMinúsculo
```

## `#!hs main`
Até agora temos usado as funções que escrevemos apenas dentro do GHCi, de modo interpretado, o que demanda uma instalação do interpretador.
Quando escrevemos um programa para ser executado em outras máquinas, normalmente queremos que ele seja um executável auto-contido, isto é, que execute independentemente de haver ou não um interpretador Haskell em tais máquinas.
Para isso, devemos **compilar** o programa e gerar um arquivo executável.

Quando compilamos um programa Haskell, este deve ter uma função `#!hs main` (em um módulo que dever explicitamente denominado `#!hs Main`, caso tenha mais de um módulo), e que será sempre a função invocada quando o executável for invocado.
Como ações de E/S só podem ser executadas dentro de outras ações de E/S, a função `#!hs main` deve ser uma ação E/S também.

```hs
main :: IO ()
main = do
        putStrLn "Digite seu nome: "
        nome <- getLine
        putStrLn $ "Olá, " ++ nome
```

```
>> ghc --make main.hs
[1 of 1] Compiling Main             ( main.hs, main.o )
Linking main ...
>> ./main            
Digite seu nome: 
Asdrubalino
Olá, Asdrubalino
```
