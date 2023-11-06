# Blockchain-Ocaml 🐪 ⛓️

❓**What is this project:** This is a decentralized blockchain that records transactions for a cryptocurrency implemented in OCaml 🐪 using a functional programming paradigm.

🧰:**Libraries used:** **OUnit2**:for testing | **Cryptokit**: for hash function (Sha256) | **Nocrypto**: for generating public and private keys for users with RSA.

**🛠️Modules :** Users, Transaction, Block, Blockchain -> each module defines its own type along with functions that can be performed on each type**

<h3>❗Important information:</h3>
**Transactions have their own hash function thus before a block is hashed it contains an already hashed transaction. Time is included in these hashes thus, if blockB has the same contents as a blockA but blockB was created a second before or after blockA then they will have completely different hashes making the blockchain immutable. **

<h3>🧐Why functional progamming:</h3>
**The main reason was because of the limited number of possible side effects. It is much more predictable and easier to debug because most of the functions are pure. I did not implement any reference types making it easier to implement immutable data structures which is an essential part of any secure blockchain (although it is possible that some of the libraries used might have a reference type somewhere in their implementation). Lastly OCaml is a really fun programming language with things like pipelining, pattern matching, and type inference making the coding experience enjoyable🌟😄.**
