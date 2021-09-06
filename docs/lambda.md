# Cálculo  $\lambda$

O cálculo $\lambda$ ($\lambda$-calculus) é um sistema formal para expressão computação desenvolvido pelo matemático Alonzo Church em 1930 e que serve de base para o paradigma de programação funcional.
O formalismo em si é muito simples e consiste na descrição da computação executada na forma de termos lambda, que podem dos seguintes tipos:

* constante: por exemplo, 4;[^lambda1]
* variável: por exemplo, $x$;
* abstração: se $t$ é um termo $\lambda$ e $x$ é uma variável, então $(\lambda x.t)$ é uma abstração, que define uma função onde $x$ é um parâmetro a ser aplicado em $t$, por exemplo, a abstração $\lambda x.x*x$ é função $f(x) = x^2$;
* aplicação: se $t$ e $s$ são termos, então $(ts)$ é uma aplicação, que representa a invocação da função $t$ com parâmetro $s$, por exemplo, $((\lambda x.x*x)3)$ corresponde a invocação de $f(3)$, onde $f(x) = x^2$.


O cálculo da computação acontece pela aplicação sucessiva de certas regras de transformação que reduzem ("simplificam") os termos até que um resultado seja obtido.
As reduções $\beta$ são as mais importantes no contexto do nosso estudo, pois elas reduzem uma aplicações.
Por exemplo, seja o termo lambda $(\lambda x.2 \times x)5$; a redução $\beta$ reduz o termo pela termo resultante da substituição de todas as ocorrências de $x$ por 5, que pode ser novamente substituído pela aplicação do operador $\times$, isto é 

$$
\begin{eqnarray}
(\lambda x.2 \times x)5 &\rightarrow_\beta& 5 \times 2 \\
                        &\rightarrow& 10\\
\end{eqnarray}
$$

Algo muito importante na $\beta$ redução é que qualquer termo pode ser usado na substituição de uma variável, mesmo um termo que represente outra função, como na redução seguinte.

$$
\begin{eqnarray}
(\lambda f.f~5) (\lambda x.x + 1) &\rightarrow_\beta& (\lambda x.x + 1)5\\
                        &\rightarrow_\beta& 5 + 1\\
                        &\rightarrow& 6\\
\end{eqnarray}
$$

Isso é o que se denomina considerar funções como **cidadãos de primeira classe** (do inglês, *first class citizens*), algo equivalente a dizer que funções também também são dados.

Em termos mais complexos, haverão múltiplas possibilidades de redução $\beta$, como no exemplo seguinte, onde há duas possibilidades.
Façamos primeiro a redução da expressão mais interna.

$$
\begin{eqnarray}
(\lambda x.\lambda y.x + ((\lambda x.x - 3) y)) 5~6 &\rightarrow_\beta& (\lambda x.\lambda y.x + (y - 3)) 5~6\\
                        &\rightarrow_\beta& (\lambda y.5 + (y - 3)) 6 \\
                        &\rightarrow_\beta& 5 + (6 - 3) \\
                        &\rightarrow& 5 + 3\\
                        &\rightarrow& 8\\
\end{eqnarray}
$$

A outra possibilidade é fazer a redução primeiro da expressão mais externa, mas neste caso é necessário observar que há dois $x$ distintos no termo, e que somente um será substituído por 5.

$$
\begin{eqnarray}
(\lambda x.\lambda y.x + ((\lambda x.x - 3) y)) 5~6 &\rightarrow_\beta& (\lambda y.5 + ((\lambda x.x - 3) y))6 \\
                                                    &\rightarrow_\beta& 5 + ((\lambda x.x - 3) 6) \\
                                                    &\rightarrow_\beta& 5 + (6 - 3) \\
                                                    &\rightarrow& 5 + 3 \\
                                                    &\rightarrow& 8 
\end{eqnarray}
$$

Apesar da ordem de redução diferente, o resultado deve ser obviamente o mesmo.

!!!exercise "Exercício"
    Reduza o termo lambda $((\lambda x.(\lambda y. x + y)5) ((\lambda y.y-3)7))$

    ???example "Resolução"
        9      


## Relação com Haskell
"Mas por quê complicar tanto?", você me pergunta; "Haskell não é complicado o suficiente para você?"
O objetivo não é complicar, mas mostrar como o cálculo $\lambda$ é poderoso para representar computações e, por conseguinte, como o Haskell também é.
De fato, os conceitos discutidos até agora são a base das seguintes funcionalidades do Haskell.

* Funções de alta ordem
* Funções lambda (funções anônimas)
* Currying
* Avaliação preguiçosa




[^lambda1]: Na verdade este não é um termo lambda, mas uma simplificação de como um termo lambda poderia ser usado para representar uma constante, não somente numérica mas também, por exemplo, booleanos. A forma purista do cálculo $\lambda$ seria muito complicada para este curso.










