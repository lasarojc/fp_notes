type Pessoa = (String, String, String, String)

fazPessoa :: String -> String -> String -> String -> Pessoa -- (1) Definição da função.
fazPessoa nome telefone cpf endereço = (nome, telefone, cpf, endereço)

pegaNome :: Pessoa -> String
pegaNome (nome, _, _, _) = nome

pegaTelefone :: Pessoa -> String
pegaTelefone (_, telefone, _, _) = telefone

trocaTelefone :: Pessoa -> String-> Pessoa
trocaTelefone (n, _t, c, e) novoTelefone = (n, novoTelefone, c, e)
