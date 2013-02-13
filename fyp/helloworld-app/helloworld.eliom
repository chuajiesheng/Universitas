{shared{
  open Eliom_lib
  open Eliom_content
}}

module Helloworld_app =
  Eliom_registration.App (
    struct
      let application_name = "helloworld"
    end)

let main_service =
  Eliom_service.service ~path:[] ~get_params:Eliom_parameter.unit ()

let rec fact = function
  | 0 -> 1
  | n -> n * fact(n-1)

let () =
  Helloworld_app.register
    ~service:main_service
    (fun () () ->
      Lwt.return
        (Eliom_tools.F.html
           ~title:"helloworld"
           ~css:[["css";"helloworld-app.css"]]
           Html5.F.(body [
             h2 [pcdata "Welcome from Eliom's destillery!"];
             p [pcdata(string_of_int(fact(10)))];
           ])))
