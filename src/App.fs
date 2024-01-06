// taidalog's portfolio Version 0.1.1
// https://github.com/taidalog/taidalog.github.io
// Copyright (c) 2023 taidalog
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
            // snow falling
            fall 0.
            fall 1.
            fall 2.
            fall 4.
            fall 8.
            fall 16.)
    )
