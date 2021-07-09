fazPessoa :: String -> String -> String -> String -> (String, String, String, String) -- (1) Definição da função.
fazPessoa nome telefone cpf endereço = (nome, telefone, cpf, endereço)

pegaNome :: (String, String, String, String) -> String
pegaNome (nome, _, _, _) = nome

pegaTelefone :: (String, String, String, String) -> String
pegaTelefone (_, telefone, _, _) = telefone

trocaTelefone :: (String, String, String, String) -> String-> (String, String, String, String)
trocaTelefone (n, _t, c, e) novoTelefone = (n, novoTelefone, c, e)
