open Tea.App
open Tea.Html

type msg =
  | MouseMove of int * int
  | Click


type coords = int * int
module Color = struct
    type t = 
      | Red | Green | Blue | White | Yellow

    let next = function
        | Red -> Green
        | Green -> Blue
        | Blue -> White
        | White -> Yellow
        | Yellow -> Red
    
    let to_rgb = function
        | Red -> "#ff0000"
        | Green -> "#00ff00"
        | Blue -> "#0000ff"
        | White -> "#ffffff"
        | Yellow -> "#ffff00"

end
module Model = struct
    type t = {
        coords: coords;
        color: Color.t
    }

    let init () = 
        (
            {coords = (0, 0); color = Color.Red},
            Tea.Cmd.none
        )

    let update model msg = 
        let new_model = match msg with
        | MouseMove (x, y) -> { model with coords = (x, y) }
        | Click -> { model with color = (Color.next model.color) }
        in
        (new_model, Tea.Cmd.none)
end

[@@@ warning "-27"]
let subscriptions model =
    Tea.Sub.batch [
        Tea.Mouse.moves (fun mouse_pos -> MouseMove (mouse_pos.x,mouse_pos.y)) ;
        Tea.Mouse.ups (fun _ -> Click)
    ]
[@@@ warning "+27"]

module View = struct
    open Model

    let play_field = div [
        id "playField" ; 
        style "width" "100%" ; 
        style "height" "100%" ;
        style "background-color" "black" ;
        style "margin" "0" 
    ]

    let square model = 
        let x,y = model.coords in
        div [ 
            id "playerSquare" ; 
            style "background-color" (Color.to_rgb model.color) ; 
            style "height" "50px" ; 
            style "width" "50px" ; 
            style "position" "absolute" ;
            style "top" (y - 25 |> string_of_int) ;
            style "left" (x - 25 |> string_of_int) 
        ] []

    let render model = 
        play_field [ square model ]
end

let main = standardProgram {
    init = Model.init ;
    update = Model.update ; 
    view = View.render ;
    subscriptions
}

