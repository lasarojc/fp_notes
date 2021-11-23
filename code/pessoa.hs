fazPessoa nome telefone cpf endereço = (nome, telefone, cpf, endereço)

pegaNome (nome, _, _, _) = nome

pegaTelefone (_, telefone, _, _) = telefone

trocaTelefone (n, _t, c, e) novoTelefone = (n, novoTelefone, c, e)
