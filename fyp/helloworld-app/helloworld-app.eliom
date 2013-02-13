{shared{
  open Eliom_lib
  open Eliom_content
}}

module Helloworld-app_app =
  Eliom_registration.App (
    struct
      let application_name = "helloworld-app"
    end)

let main_service =
  Eliom_service.service ~path:[] ~get_params:Eliom_parameter.unit ()

let () =
  Helloworld-app_app.register
    ~service:main_service
    (fun () () ->
      Lwt.return
        (Eliom_tools.F.html
           ~title:"helloworld-app"
           ~css:[["css";"helloworld-app.css"]]
           Html5.F.(body [
             h2 [pcdata "Welcome from Eliom's destillery!"];
           ])))
