type Pessoa = (String, String, String, String)

fazPessoa :: String -> String -> String -> String -> Pessoa -- (1) Definição da função.
fazPessoa nome telefone cpf endereço = (nome, telefone, cpf, endereço)

pegaNome :: Pessoa -> String
pegaNome (nome, _, _, _) = nome

pegaTelefone :: Pessoa -> String
pegaTelefone (_, telefone, _, _) = telefone

pegaCPF :: Pessoa -> String
pegaCPF (_, _, c, _) = c

pegaEndereço :: Pessoa -> String
pegaEndereço (_, _, _, e) = e

trocaTelefone :: Pessoa -> String-> Pessoa
trocaTelefone p novoTelefone = fazPessoa (pegaNome p) novoTelefone (pegaCPF p) (pegaEndereço p)
