# Desenvolvimento de Software em Haskell
O desenvolvimento de software, em qualquer paradigma e usando qualquer linguagem, requer organização para que tarefa não gere artefatos de difícil uso e manutenção.

Um dos pontos principais desta organização é a divisão do código em módulos contendo partes do código com forte associação, como classes que descrevem um domínio de problema, quer fornecem uma funcionalidade, ou que podem, ao menos em teoria, ser usados independentemente de outros módulos.
Assim, módulos facilitam o gerenciamento da complexidade inerente dos artefatos de software.

A modularização pode acontecer em vários níveis. Por exemplo, um módulo que fornece funcionalidades de manipulação de interfaces gráficas, pode ter um submódulos específicos para diferentes placas gráficas, ou uma biblioteca para comunicação entre processos via rede de computadores pode ser dividida em comunicação segura e não segura.

Embora os softwares que desenvolveremos neste curso sejam pequenos e de baixa complexidade, precisamos pensar em modularização tanto para a criação de bons hábitos mas também para usarmos a vasta biblioteca disponível para projetos nesta linguagem.

Outro aspecto importante do desenvolvimento são os testes, que aumentam a confiança na corretude do código. Novamente temos opções para o desenvolvimento de testes.