# Compiladores 2

Projeto para a disciplina de compiladores 2


## Como executar o projeto

Instale [Haskell](https://www.haskell.org/platform/)

Instale o [stack](https://docs.haskellstack.org/en/stable/install_and_upgrade/)

Execute os seguintes comando

```sd
cd haskell
# Gerar o codigo intermediario
stack run ./exemplo.lalg.txt > output.txt
# Interpretar o codigo gerado
stack run interpreter ./output.txt
```

## Automato do analisador léxico

![alt text](https://github.com/MarcusGoldschmidt/compiladores-1/blob/master/haskell/automato.png)

## Gramática com as regras semânticas

```
<programa> -> program ident <corpo> .
<corpo> -> <dc> begin <comandos> end
<dc> -> <dc_v> <mais_dc>  | λ
<mais_dc> -> ; <dc> | λ
<dc_v> ->  <tipo_var> : <variaveis>
<tipo_var> -> real | integer
<variaveis> -> ident <mais_var>
<mais_var> -> , <variaveis> | λ
<comandos> -> <comando> <mais_comandos>
<mais_comandos> -> ; <comandos> | λ

<comando> ->    read (ident) |
                write (ident) |
                ident := <expressao> |
                if <condicao> then <comandos> <pfalsa> $
							
<condicao> -> <expressao> <relacao> <expressao>
<relacao> -> = | <> | >= | <= | > | <
<expressao> -> <termo> <outros_termos>
<termo> -> <op_un> <fator> <mais_fatores>
<op_un> -> - | λ
<fator> -> ident | numero_int | numero_real | (<expressao>)
<outros_termos> -> <op_ad> <termo> <outros_termos> | λ
<op_ad> -> + | -
<mais_fatores> -> <op_mul> <fator> <mais_fatores> | λ
<op_mul> -> * | /
<pfalsa> -> else <comandos> | λ

```
