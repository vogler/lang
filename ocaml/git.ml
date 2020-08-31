#use "topfind"
#require "batteries"
open Batteries

let sha1 = String.of_int % Hashtbl.hash (* TODO this is not sha1 *)

(* according to https://youtu.be/2sjqTHE0zok?t=1413 *)

type blob = string list

type tree = (string, [`Tree of tree | `Blob of blob]) Map.t

type commit = {
  parents: commit list;
  author: string;
  message: string;
  snapshot: tree;
}

type obj = [`Blob of blob | `Tree of tree | `Commit of commit]

type objects = (string, obj) Hashtbl.t
let objects = Hashtbl.create 123

let store o =
  let id = sha1 o in
  Hashtbl.add objects id o

let load id = Hashtbl.find objects id

let references : (string, string) Hashtbl.t = Hashtbl.create 123
