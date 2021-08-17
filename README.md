# sparser-invaders

### A haskell CLI app to parse json files and translate them into js objects and/or yml files using compiler techniques and tools
#### [LEXICAL ANALYSIS] - Tokenizing symbols in
##### 
`` OpenObjToken       | 
  CloseObjToken      | 
  IdentifierKeyToken |
  StringToken        |
  NumberToken        |
  NullToken          |
  OpenArrayToken     |
  CloseArrayToken    |
  BooleanToken       |
  SeparatorToken     |
  Empty --end of file
  ``

<img src="https://i.postimg.cc/PqY691f5/lexerror.jpg">
 
#### [SINTATIC ANALYSIS] - Using free context grammar to check if tokens are in the right place 
##### ``A -> [C] | {K} | {} | []``
##### ``C -> TEC | T``
##### ``K -> "key":TEK | "key":T``
##### ``E -> ,``
##### ``T -> "str" | 0-9 | 0.0-9.9 | A | true | false | null``

<img src="https://i.postimg.cc/Wb1HzD85/sintatic-error.jpg">

#### [SEMANTIC ANALYSIS] - As warnings to inform the user of duplicated keys in the same scope

<img src="https://i.postimg.cc/4xrLWWg0/warnings.jpg">

#### [CODE GEN] - From the tokens IR is built and soon transformed in a yml and/ or js file

<img src="https://i.postimg.cc/vZ127pp9/success.jpg">
