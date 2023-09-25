module List = struct
  include List

  let count p xs = fold_left (fun n x -> if p x then n + 1 else n) 0 xs

  let remove idx items =
    let rec loop i items =
      match (i, items) with
      | 0, _ :: xs -> xs
      | i, x :: xs -> x :: loop (i - 1) xs
      | _, [] -> []
    in
    loop idx items

  let update i f = mapi (fun j x -> if i = j then f x else x)

  let fold_lefti f init list =
    let _, result =
      List.fold_left (fun (i, accum) elem -> (i+1, f i accum elem)) (0, init) list
    in
    result
end

module App = struct
  type item = { title : string; completed : bool }

  type editing_state =
    | NoEditing
    | Editing of { idx : int; new_title : string }

  type visibility = All | Active | Completed

  type state = {
      new_item_text : string;
      items : item list;
      editing : editing_state;
      visibility : visibility;
    }

  let count_incomplete state =
    List.count (fun item -> not item.completed) state.items

  let is_nil = function [] -> true | _ -> false

  type action =
    | UpdateNew of string
    | AddItem
    | RemoveCompleted
    | RemoveIdx of int
    | SetCompletionStatus of int * bool
    | SelectForEditing of int
    | UpdateEdit of string
    | FinishEditing of [ `reset | `keep ]
    | SetVisibility of visibility

  module Html = Ulmus.Html
  let concat = Html.concat_list
  module A = Html.A
  module E = Html.E

  let todo_item idx editing { title; completed } =
    Html.li
      ~attrs:[ E.ondoubleclick (SelectForEditing idx) ]
      (concat
         [ if completed then
             Html.input
               ~attrs:
               [
                 A.type_ "checkbox";
                 A.checked true;
                 E.oninput (fun _ -> SetCompletionStatus (idx, false));
               ]
           else
             Html.input
               ~attrs:
               [
                 A.type_ "checkbox";
                 E.oninput (fun _ -> SetCompletionStatus (idx, true));
               ];
           (match editing with
            | Some new_title ->
               Html.input
                 ~attrs:
                 [
                   A.type_ "text";
                   A.value new_title;
                   A.autofocus;
                   E.blur (FinishEditing `keep);
                   E.oninput (fun s -> UpdateEdit s);
                   E.onkeydown (fun _mods key ->
                       match key with
                       | Js_of_ocaml.Dom_html.Keyboard_code.Enter ->
                          Some (FinishEditing `keep)
                       | Js_of_ocaml.Dom_html.Keyboard_code.Escape ->
                          Some (FinishEditing `reset)
                       | _ -> None);
                 ]
            | None ->
               Html.label
                 ~attrs:[ A.class_ (if completed then "done" else "todo") ]
                 (Html.text title));
           Html.button ~attrs:[ E.onclick (RemoveIdx idx) ] (Html.text "❌")
      ])

  let items state =
    List.fold_lefti
      (fun idx d item ->
        let is_editing =
          match state.editing with
          | NoEditing -> None
          | Editing { idx = editing_idx; new_title } ->
             if idx = editing_idx then Some new_title else None
        in
        match (state.visibility, item.completed) with
        | All, _ | Completed, true | Active, false ->
           Html.( ^^ ) d (todo_item idx is_editing item)
        | Completed, false | Active, true -> d)
      Html.empty
      state.items

  let render state =
    Html.section
      ~attrs:[ A.class_ "todo-list" ]
      (concat
         [ Html.header
             (Html.input
                ~attrs:[
                  A.placeholder "What needs to be done?";
                  A.value state.new_item_text;
                  A.type_ "text";
                  E.oninput (fun s -> UpdateNew s);
                  E.onkeydown (fun _mods key ->
                      match key with
                     | Js_of_ocaml.Dom_html.Keyboard_code.Enter -> Some AddItem
                     | Js_of_ocaml.Dom_html.Keyboard_code.Escape ->
                         Some (UpdateNew "")
                     | _ -> None);
               ]);
        Html.main
          (if is_nil state.items then Html.empty else Html.ul (items state));
        Html.footer
          (let num_incomplete = count_incomplete state in
           concat
             [ Html.span
                 (Html.text
                    (Printf.sprintf "%d item%s remaining" num_incomplete
                       (if num_incomplete = 1 then "" else "s")));
               Html.button
                 ~attrs:[ E.onclick (SetVisibility All) ]
                 (Html.text "All");
               Html.button
                 ~attrs:[ E.onclick (SetVisibility Active) ]
                 (Html.text "Active");
               Html.button
                 ~attrs:[ E.onclick (SetVisibility Completed) ]
                 (Html.text "Completed");
               Html.button
                 ~attrs:
                 [
                   E.onclick RemoveCompleted;
                   A.disabled (num_incomplete = List.length state.items);
                ]
                 (Html.text "Clear completed")
          ])
      ])

  let update action state =
    match action with
    (* Adding *)
    | UpdateNew s -> { state with new_item_text = s }
    | AddItem ->
        let title = String.trim state.new_item_text in
        if title = "" then state
        else
          let item = { title; completed = false } in
          {
            state with
            new_item_text = "";
            items = state.items @ [ item ];
            editing = NoEditing;
          }
    (* Editing *)
    | SetCompletionStatus (idx, completed) ->
        {
          state with
          items =
            List.update idx (fun item -> { item with completed }) state.items;
        }
    | FinishEditing `keep -> (
        match state.editing with
        | Editing { idx; new_title } ->
            let title = String.trim new_title in
            if title = "" then
              {
                state with
                items = List.remove idx state.items;
                editing = NoEditing;
              }
            else
              {
                state with
                items =
                  List.update idx (fun item -> { item with title }) state.items;
                editing = NoEditing;
              }
        | NoEditing -> state)
    | FinishEditing `reset -> { state with editing = NoEditing }
    | SelectForEditing idx -> (
        match List.nth_opt state.items idx with
        | None -> state
        | Some { title; _ } ->
            { state with editing = Editing { idx; new_title = title } })
    | UpdateEdit new_title -> (
        match state.editing with
        | NoEditing -> state
        | Editing x -> { state with editing = Editing { x with new_title } })
    (* Removing *)
    | RemoveCompleted ->
        {
          state with
          items = List.filter (fun item -> not item.completed) state.items;
          editing = NoEditing;
        }
    | RemoveIdx idx -> { state with items = List.remove idx state.items }
    (* Filtering *)
    | SetVisibility visibility -> { state with visibility }

  let initial =
    {
      new_item_text = "";
      editing = NoEditing;
      items =
        [
          { title = "Item A"; completed = false };
          { title = "Item B"; completed = true };
          { title = "Item C"; completed = false };
        ];
      visibility = All;
    }
end

let () =
  Ulmus.attach ~parent_id:"todo-app" ~initial:App.initial (module App)
