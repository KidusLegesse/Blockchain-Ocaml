open OUnit2
open Blockchain

(*This File tests the Modules in Blokchain.ml and allows users to interact with,
  create, and display their own blockchains*)
let make_new_transactions sender reciver amount =
  Transaction.make_transation (User.create_user sender)
    (User.create_user reciver) amount

let mine_new_block index transactions prev_hash proof_value =
  let block = Block.new_block index transactions prev_hash proof_value in
  Block.mine_block block block.proof_value

let make_new_chain chain block = Blockchain.add_block chain block

(*Transaction lists*)
let transactions =
  [
    make_new_transactions "Alex" "Lane" 10.;
    make_new_transactions "Bob" "Charlie" 5.38;
  ]

let transactions_2 =
  [
    make_new_transactions "Mario" "Toad" 16.;
    make_new_transactions "John" "Eric" 90.58;
    make_new_transactions "Ash" "Pikachu" 45.;
    make_new_transactions "Dory" "Mario" 60.;
  ]

let transactions_3 = [ make_new_transactions "Dave" "Sara" 11.25 ]

let transactions_4 =
  [
    make_new_transactions "Taylor" "Lexi" 10.6;
    make_new_transactions "Lexi" "Toad" 31.;
    make_new_transactions "Toad" "Pikachu" 44.25;
  ]

let transactions_5 =
  [
    make_new_transactions "Shawn" "Tyler" 17.5;
    make_new_transactions "Tyler" "Dory" 55.;
    make_new_transactions "Charlie" "Alex" 47.30;
    make_new_transactions "Jake" "Ash" 29.;
  ]

let transactions_6 =
  [
    make_new_transactions "Mike" "Frank" 27.;
    make_new_transactions "Kenny" "Joy" 6.15;
    make_new_transactions "Samantha" "Rob" 60.77;
    make_new_transactions "Julie" "Shawn" 31.25;
    make_new_transactions "Samantha" "jake" 30.;
  ]

(*Creating and mining Blocks*)
let block = mine_new_block 1 transactions "" 0
let block_2 = mine_new_block 2 transactions_2 "" 0
let block_3 = mine_new_block 3 transactions_3 "" 0
let block_4 = mine_new_block 4 transactions_4 "" 0
let block_5 = mine_new_block 5 transactions_5 "" 0
let block_6 = mine_new_block 6 transactions_6 "" 0

(*Creates new blockchain*)
let mainchain = make_new_chain [] block
let main = make_new_chain mainchain block_2
let main_2 = make_new_chain main block_3
let main_3 = make_new_chain main_2 block_4
let main_4 = make_new_chain mainchain block_5
let main_5 = make_new_chain main_4 block_6

let blockchain_test _ =
  assert_equal (List.hd Blockchain.initial_chain).id 0
    ~msg:"Testing  blockchain equility";
  assert_equal (Block.valid_block block_3) true;
  assert_equal (Block.hash_block block_3 != Block.hash_block block_4) true
(* assert_equal (Block.hash_block (List.hd Blockchain.initial_chain) = (List.hd
   main_2).previous_hash) true ~msg:"Testing blockchain equility" *)

let suite = "Blockchain Tests" >::: [ "main function" >:: blockchain_test ]

let () =
  Blockchain.show_blockchain_history main;
  Blockchain.show_blockchain_history main_2;
  Blockchain.show_blockchain_history main_3;
  Blockchain.show_blockchain_history main_4;
  Blockchain.show_blockchain_history main_5;
  run_test_tt_main suite
