type Nome = (String, String, String)
type Telefone = (String, String)
type CPF = String
type Endereço = (String, String, String)
type Pessoa = (Nome, Telefone, CPF, Endereço)

fazPessoa :: Nome -> Telefone -> CPF -> Endereço -> Pessoa -- (1) Definição da função.
fazPessoa nome telefone cpf endereço = (nome, telefone, cpf, endereço)

pegaNome :: Pessoa -> Nome
pegaNome (nome, _, _, _) = nome

pegaTelefone :: Pessoa -> Telefone
pegaTelefone (_, telefone, _, _) = telefone

trocaTelefone :: Pessoa -> Telefone -> Pessoa
trocaTelefone (n, _t, c, e) novoTelefone = (n, novoTelefone, c, e)
