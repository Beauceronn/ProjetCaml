type int_inf = int list;;

(* on ?crit les chiffres les plus significatif ? gauche ainsi 
  le nombre 98987687667576 sera vu comme la liste 
  [98;9876;8766;7576] 
  0 est cod? comme la liste [0] pas la liste vide !
  un probl?me : [123;0;67;43] se d?code en 123000000670043. Il faut faire 
  attention a ne pas oublier les 0 intermediaires lors du decodage. 
  Les entiers negatifs sont encodes en mettant le chiffre le plus a gauche 
  comme negatif. Ainsi -98987687667576 sera vu comme la liste
  [-98;9876;8766;7576] *)

let base : int = 10000;;

let rec normal (l:int_inf) : int_inf =
  (* (normal l) = la liste dont on a enlev? les 0 non significatifs 
       gauche. Par exemple (normal[0;0;1234;5678]=[1234;5678]) *)
match l with
|[] -> []
|e::suite -> if (e = 0) then normal suite else e::suite
;; 

let parite (b:int_inf) : bool = 
  (* (parite b) est vrai ssi b est un nombre pair *) // on accepte ou pas la liste vide ?
match b with
|[] -> false
|_ -> ((List.hd (List.rev b)) mod 2) == 0
;;

let egal_inf (a : int_inf)  (b:int_inf) : bool = 
  (* (egal_inf a b) est vrai ssi a egal b *) 
 (normal a) = (normal b) 
;;

---------------------------------------------------------------------
(* Fonctions *)
let som = fun e1 e2 -> e1 + e2;;
let modBase = fun e -> e mod base;;

(* Modules *)
let abs_inf (a : int_inf) : int_inf =
	abs(List.hd a)::(List.tl a)
;;

let retenue_pos ( a : int_inf) : int_inf = (List.map (fun i -> i/base) a)@[0];;

let rec retenue_neg( a : int_inf) : int_inf = 
	match a with	
	|[] -> []
	|e::[] -> e::[]
	|e1::e2::[] -> 	if (e1>0 && e2<0) then 
						(e1-1)::(e2+base)::[]
					else 
						e1::e2::[]
	|e3::e4::suite -> 	if (e3>0 && e4 <0) then 
							(e3-1)::(retenue_neg ((e4+base)::suite))
						else
							e3::(retenue_neg (e4::suite))

;;

let tout_neg( a: int_inf):int_inf=
	(List.hd a)::(List.map (fun e -> -e)(List.tl a));;

let som_list(a : int_inf) (b:int_inf) : int_inf =
 	List.map2 (fun e1 e2 -> e1 + e2) a b;;

let rec complement ( a : int_inf) (dif : int) : int_inf= 
		match dif with
		|0 -> []@a
		|_ ->[0]@(complement a (dif-1));;

(* Cas différents *)

let som_2pos_inf ( a : int_inf)  ( b :int_inf) : int_inf = 
	let la = (List.length a) in 
	let lb = (List.length b) in
		if ( la == lb) then
			normal
				(som_list
					(List.map modBase (List.map2 som ([0]@a) ([0]@b)))
					(retenue_pos (List.map2 som a b))
				)
		else if (la < lb) then 
			normal
				(som_list
					(List.map modBase (List.map2 som ([0]@(complement a (abs(la - lb)))) ([0]@b)))
					(retenue_pos (List.map2 som (complement a (abs(la - lb))) b))
				)
				
				else
					normal
						(som_list 
							(List.map modBase (List.map2 som ([0]@(complement b (abs(la - lb)))) ([0]@a)))
							(retenue_pos (List.map2 som (complement b (abs(la - lb))) a))
						);;



let som_pos_neg_inf ( a : int_inf)  ( b :int_inf) : int_inf = 
		if (egal_inf  a (abs_inf b)) then [0]		
		else retenue_neg (som_2pos_inf a (tout_neg b))
;;

let som_2neg_inf ( a : int_inf)  ( b :int_inf) : int_inf = 
	let abs_a = abs_inf a in
	let abs_b = abs_inf b in
		(-(List.hd (som_2pos_inf abs_a abs_b)))::(List.tl (som_2pos_inf abs_a abs_b))
;;
---------------------------------------------------------------------

let som_inf  ( a : int_inf)  ( b :int_inf) : int_inf = 
	let hda = List.hd a in
	let hdb = List.hd b in
	match (a,b) with
	|([],c) -> c
	|(d,[]) -> d
	|_ -> 	if (hda>0 && hdb>0) then som_2pos_inf a b
			else if (hda<0 && hdb>0) then som_pos_neg_inf b a
				else if (hda>0 && hdb<0) then som_pos_neg_inf a b
					else som_2neg_inf a b
;;

---------------------------------------------------------------------
let hdneg (a:int_inf) :int_inf =
(-(List.hd a))::(List.tl a);;

let sub_inf (a : int_inf)  (b:int_inf) : int_inf = 
  (* (sub_inf a b) = a - b *)
    let hda = List.hd a in
	let hdb = List.hd b in
	match (a,b) with
	|([],c) -> c
	|(d,[]) -> d
	|_ -> 	if (hda>0 && hdb>0) then som_pos_neg_inf a (hdneg b) 
			else if (hda<0 && hdb>0) then som_2neg_inf  a (hdneg b)
				else if (hda>0 && hdb<0) then som_2pos_inf a (abs_inf b )
					else som_pos_neg_inf (abs_inf b) a
;;
---------------------------------------------------------------------
 let mul = fun e1 e2 -> e1*e2
 let rec unilist (e:int)(n:int):int_inf =
	match n with
	|0 -> []
	|n -> e::(unilist e (n-1))
	
;;

let rec mul_brut_inf (a:int_inf) (b:int_inf) =
	match (a,b) with
	|(a,[])-> []
    |([],b)-> []
	|(e1::suite1,e2::suite2) -> (List.fold_left2 (fun init e1 e2 -> (init+e1*e2)) 0 (unilist e2 (List.length a)) b)::(mul_brut_inf suite1 suite2)
let rec mul_inf(a : int_inf)  (b:int_inf) : int_inf = 
  (* (mul_inf a b) = a * b *)
  
;;

let rec quo_inf(a : int_inf)  (b:int_inf) : int_inf = 
  (* (quo_inf a b) = q avec a=b*q+r et r<b *)
...;;

let rec mod_inf (a:int_inf) (b:int_inf) : int_inf = 
  (* (mod_inf a b) = r avec a=b*q+r et r<b *)  
...;;

let rec exp_inf (a:int_inf) (b:int_inf) : int_inf = 
  (* (exp_inf a b) = a^b *)
...;;

let rec exp_mod_inf (a:int_inf) (b:int_inf) (m:int_inf): int_inf = 
  (* (exp_mod_inf a b m) = a^b modulo m*)
  if (egal_inf normal(b) [0]) 
     then [1]
     else if (parite b) 
             then (normal (exp_mod_inf (normal (mod_inf (mul_inf a a) m)) 
                                       (normal (quo_inf b [2])) 
                                       m))
             else (normal (mod_inf (mul_inf a 
                                            (exp_mod_inf (normal (mod_inf (mul_inf a a) m)) 
                                                         (normal (quo_inf b [2])) 
                                                          m)) 
                                     m));;

let premier_inf  (p: int_inf) : bool = function 
  (* fonction de test de primalit? bas?e sur le petit th?or?me de fermat 
     qui ?nonce que si a et p premier entre eux alors (a exp (p-1))= 1
     modulo p). Ce test est probabiliste car on ne teste que quatre
     t?moins, les m?mes que le logiciel PGP*)
   let pmoinsun = normal(sub_inf(p,[1])) in 
       egal_inf([1],exp_mod_inf([2],pmoinsun,p)) &&
       egal_inf([1],exp_mod_inf([3],pmoinsun,p)) &&
       egal_inf([1],exp_mod_inf([5],pmoinsun,p)) &&
       egal_inf([1],exp_mod_inf([7],pmoinsun,p)) ;;

let rec euclide_etendu (a:int_inf) (b:int_inf) : (int_inf * int_inf)= 
   if (egal_inf [0] mod_inf(a,b)) 
      then ([0],[1])
      else let (x,y)=(euclide_etendu b (mod_inf a b)) in 
                 (y, (sub_inf x (mul_inf y quo_inf(a,b))));;

(* une page pour trouver des nombres premiers 
    http://primes.utm.edu/lists/small/small.html#40 *)
