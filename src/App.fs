// taidalog's portfolio Version 0.2.0
// https://github.com/taidalog/taidalog.github.io
// Copyright (c) 2023-2024 taidalog
// This software is licensed under the MIT License.
// https://github.com/taidalog/taidalog.github.io/blob/main/LICENSE

namespace TaidalogsPortfolio

open Browser.Dom
open Browser.Types
open Fable.Core
open Fable.Core.JsInterop
open SnowFlake

module App =

    let keyboardshortcut (e: KeyboardEvent) =
        let informationPolicyWindow = document.getElementById "informationPolicyWindow"

        let isInformationPolicyWindowActive =
            informationPolicyWindow.classList
            |> (fun x -> JS.Constructors.Array?from(x))
            |> Array.contains "active"

        match e.key with
        | "Escape" ->
            if isInformationPolicyWindowActive then
                informationPolicyWindow.classList.remove "active"
        | _ -> ()

    window.addEventListener (
        "DOMContentLoaded",
        (fun _ ->
            // information policy window
            (document.getElementById "informationPolicyLink").onclick <-
                fun event ->
                    event.preventDefault ()
                    (document.getElementById "informationPolicyWindow").classList.add "active"

            (document.getElementById "informationPolicyClose").onclick <-
                fun _ -> (document.getElementById "informationPolicyWindow").classList.remove "active"

            // keyboard shortcut
            document.onkeydown <- fun (e: KeyboardEvent) -> keyboardshortcut e

            (document.getElementById "snowArea")
                .setAttribute ("width", string document.body.clientWidth)

            (document.getElementById "snowArea")
                .setAttribute ("height", string document.body.clientHeight)

            let timeDOMLoaded = System.DateTime.Now

            // snow falling
            [ 0..17 ] |> List.iter (fun _ -> fall timeDOMLoaded false)

            (document.getElementById "umbrella").onclick <-
                fun _ ->
                    (document.getElementById "umbrella").classList.toggle "display-none" |> ignore
                    (document.getElementById "snowing").classList.toggle "display-none" |> ignore

                    let snowFlakeElements: Element array =
                        document.getElementsByClassName "snow-flake"
                        |> fun x -> JS.Constructors.Array?from(x)

                    snowFlakeElements
                    |> Array.iter (fun x -> x.classList.add "fading-out" |> ignore)

                    setTimeout
                        (fun _ ->
                            snowFlakeElements
                            |> Array.iter (fun x -> x.parentElement.removeChild x |> ignore))
                        3000
                    |> ignore

            (document.getElementById "snowing").onclick <-
                fun _ ->
                    (document.getElementById "snowing").classList.toggle "display-none" |> ignore
                    (document.getElementById "umbrella").classList.toggle "display-none" |> ignore

                    [ 0..17 ] |> List.iter (fun _ -> fall timeDOMLoaded false))
    )

    window.onresize <-
        fun _ ->
            (document.getElementById "snowArea")
                .setAttribute ("width", string document.body.clientWidth)

            (document.getElementById "snowArea")
                .setAttribute ("height", string document.body.clientHeight)
