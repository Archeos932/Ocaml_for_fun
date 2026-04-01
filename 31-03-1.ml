module SGF = struct
    type element =
    | Fichier of string
    | Dossier of string * element list

  let rec compter_fichier (tree:element) : int =  match tree with
    | Fichier string -> 1
    | Dossier (_, liste) -> let tailles = List.map compter_fichier liste in
          List.fold_left (fun acc x -> acc + x) 0 tailles

  let rec existe (s:string) (t:element) = match t with
    | Fichier str -> str = s
    | Dossier (str,liste) -> if (str = s) then true else let verifcation = List.map (existe s) liste in
      List.fold_left (fun acc x -> acc || x ) false verifcation

  let rec chemin (cible : string) (e : element) : string option =
    match e with
    | Fichier nom ->
        if nom = cible then Some nom else None
    | Dossier (nom, liste) ->
          if nom = cible then Some nom
          else
            List.find_map (fun enfant ->
              match chemin cible enfant with
              | Some sous_chemin -> Some (nom ^ "/" ^ sous_chemin)
              | None -> None
            ) liste

end
