open Cryptokit
open Nocrypto

module User = struct
  type t = {
    name : string;
    balance : float;
    keys : Rsa.priv * Rsa.pub;
  }

  let () = Rng.reseed (Cstruct.create 32)

  let generate_rsa_key_pair =
    let priv_key = Rsa.generate 2048 in
    let pub_key = Rsa.pub_of_priv priv_key in
    (priv_key, pub_key)

  let show_priv_key (user : t) =
    match user.keys with
    | a, _ -> a

  let show_pub_key (user : t) =
    match user.keys with
    | _, b -> b

  let create_user person =
    { name = person; balance = 70.; keys = generate_rsa_key_pair }
end

module Transaction = struct
  type t = {
    sender : User.t;
    receiver : User.t;
    amount : float;
    time : float;
  }

  let signed_transaction (transaction : t) =
    if
      transaction.sender != transaction.receiver
      && Rsa.pub_of_priv (User.show_priv_key transaction.sender)
         = User.show_pub_key transaction.sender
    then true
    else false

  let make_transation send receiv amt =
    let new_transaction =
      { sender = send; receiver = receiv; amount = amt; time = Unix.time () }
    in
    if signed_transaction new_transaction then
      let update_bal = { receiv with balance = receiv.balance +. amt } in
      { new_transaction with receiver = update_bal }
    else failwith "Invalid transaction"

  let string_of_transaction (transaction : t) : string =
    transaction.sender.name ^ transaction.receiver.name
    ^ string_of_float transaction.amount
    ^ string_of_float transaction.time

  let hash_transaction (transaction : t) =
    hash_string (Cryptokit.Hash.sha256 ()) (string_of_transaction transaction)
    |> transform_string (Hexa.encode ())

  let get_balance (transactions : t) =
    transactions.sender.name ^ ": "
    ^ string_of_float transactions.sender.balance
    ^ " " ^ transactions.receiver.name ^ ": "
    ^ string_of_float transactions.receiver.balance

  let print_transaction (transaction : t) : string =
    let orginal_time = Unix.gmtime transaction.time in
    let time_revised =
      string_of_int orginal_time.tm_mon
      ^ "/"
      ^ string_of_int orginal_time.tm_mday
      ^ "/"
      ^ string_of_int orginal_time.tm_year
      ^ " "
      ^ string_of_int orginal_time.tm_hour
      ^ ":"
      ^ string_of_int orginal_time.tm_min
      ^ ":"
      ^ string_of_int orginal_time.tm_sec
      ^ " GMT"
    in
    "{" ^ "Sender: " ^ transaction.sender.name ^ " Reiceiver: "
    ^ transaction.receiver.name ^ " Amount: "
    ^ string_of_float transaction.amount
    ^ " Time: " ^ time_revised ^ "}"
end

module Block = struct
  type t = {
    id : int;
    transactions : Transaction.t list;
    previous_hash : string;
    proof_value : int;
    time : float;
  }

  let new_block iden actions prev proof =
    {
      id = iden;
      transactions = actions;
      previous_hash = prev;
      proof_value = proof;
      time = Unix.time ();
    }

  let string_of_block ?(str : string = "") (block : t) : string =
    string_of_int block.id
    ^ String.concat str
        (List.map Transaction.hash_transaction block.transactions)
    ^ block.previous_hash
    ^ string_of_int block.proof_value
    ^ string_of_float block.time

  let hash_block (block : t) =
    let hash = Cryptokit.Hash.sha256 () in
    hash_string hash (string_of_block block)
    |> transform_string (Hexa.encode ())

  let rec mine_block ?(target = "000") block value =
    if
      String.sub
        (hash_block { block with proof_value = value })
        0 (String.length target)
      = target
    then { block with proof_value = value }
    else mine_block block (value + 1)

  let compare_blocks block block_2 =
    let block_hash = hash_block block in
    let block_2_hash = hash_block block_2 in
    block_hash = block_2_hash

  let add_transaction (block : t) (transaction : Transaction.t) =
    if Transaction.signed_transaction transaction then
      let new_list = transaction :: block.transactions in
      { block with transactions = new_list }
    else failwith "Invalid Transaction"

  let valid_block (block : t) =
    let rec valid_block_helper (lst : Transaction.t list) =
      match lst with
      | [] -> true
      | h :: t ->
          if Transaction.signed_transaction h then valid_block_helper t
          else false
    in
    valid_block_helper block.transactions
  (*&& block.proof_value = ((mine_block block block.proof_value).proof_value)*)

  let get_balance_block block =
    String.concat "; " (List.map Transaction.get_balance block.transactions)

  let print_block (block : t) : string =
    let orginal_time = Unix.gmtime block.time in
    let time_revised =
      string_of_int orginal_time.tm_mon
      ^ "/"
      ^ string_of_int orginal_time.tm_mday
      ^ "/"
      ^ string_of_int orginal_time.tm_year
    in
    "ID: " ^ string_of_int block.id ^ "\n" ^ "Transactions: ["
    ^ String.concat " ; "
        (List.map Transaction.print_transaction block.transactions)
    ^ "]" ^ "\n" ^ "Previous Hash: " ^ block.previous_hash ^ "\n"
    ^ "Block Value: "
    ^ string_of_int block.proof_value
    ^ "\n" ^ "Block creation Date: " ^ time_revised
end

module Blockchain = struct
  type t = Block.t list

  let initial_chain =
    [
      Block.new_block 0
        (Transaction.make_transation (User.create_user "first")
           (User.create_user "sec") 20.
        :: [])
        " " 0;
    ]

  let add_block (chain : t) (block : Block.t) =
    if Block.valid_block block then
      match chain with
      | [] -> block :: initial_chain
      | h :: _ -> { block with previous_hash = Block.hash_block h } :: chain
    else failwith "This block cannot be added to the blockchain"

  let get_balance_chain chain =
    String.concat "\n" (List.map Block.get_balance_block chain)

  let show_blockchain_history chain =
    print_string
      (String.concat (" -> " ^ "\n") (List.map Block.print_block chain)
      ^ "\n" ^ "Balances: " ^ get_balance_chain chain)
end
