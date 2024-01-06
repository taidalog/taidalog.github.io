// taidalog's portfolio Version 0.1.1
// https://github.com/taidalog/taidalog.github.io
// Copyright (c) 2023 taidalog
// This software is licensed under the MIT License.
// https://github.com/taidalog/taidalog.github.io/blob/main/LICENSE

namespace TaidalogsPortfolio

open Browser.Dom
open Browser.Types
open Fable.Core

module SnowFlake =
    type Point = { X: float; Y: float }

    type SnowFlake =
        { StartPoint: Point
          StopPoint: Point
          Duration: float
          Fill: string
          FontSize: int }

    let create (startPoint: Point) (stopPoint: Point) (fill: string) (fontSize: int) (duration: float) : SnowFlake =
        { SnowFlake.StartPoint = startPoint
          StopPoint = stopPoint
          Duration = duration
          Fill = fill
          FontSize = fontSize }

    let create' (startPoint: Point) (stopPoint: Point) (fill: string) (fontSize: int) : SnowFlake =
        let velocityFactor = float fontSize / 40.

        let duration =
            sqrt (pown (startPoint.X - stopPoint.X) 2 + pown (startPoint.Y - stopPoint.Y) 2)
            / 10.
            / velocityFactor

        create startPoint stopPoint fill fontSize duration

    let toElement (snowFlake: SnowFlake) (begin': float) : Element =
        let text = document.createElementNS ("http://www.w3.org/2000/svg", "text")
        text.setAttribute ("fill", snowFlake.Fill)
        text.setAttribute ("stroke", "none")
        text.setAttribute ("opacity", "0")
        text.setAttribute ("x", string snowFlake.StartPoint.X)
        text.setAttribute ("y", string snowFlake.StartPoint.Y)
        text.setAttribute ("font-size", sprintf "%dpx" snowFlake.FontSize)
        text.classList.add "snow-flake"
        text.textContent <- "*"
        let animate1 = document.createElementNS ("http://www.w3.org/2000/svg", "animate")
        animate1.setAttribute ("attributeName", "x")
        animate1.setAttribute ("values", sprintf "%f;%f" snowFlake.StartPoint.X snowFlake.StopPoint.X)
        animate1.setAttribute ("dur", sprintf "%fs" snowFlake.Duration)
        animate1.setAttribute ("begin", sprintf "%fs" begin')
        animate1.setAttribute ("fill", "freeze")
        let animate2 = document.createElementNS ("http://www.w3.org/2000/svg", "animate")
        animate2.setAttribute ("attributeName", "y")
        animate2.setAttribute ("values", sprintf "%f;%f" snowFlake.StartPoint.Y snowFlake.StopPoint.Y)
        animate2.setAttribute ("dur", sprintf "%fs" snowFlake.Duration)
        animate2.setAttribute ("begin", sprintf "%fs" begin')
        animate2.setAttribute ("fill", "freeze")
        let animate3 = document.createElementNS ("http://www.w3.org/2000/svg", "animate")
        animate3.setAttribute ("attributeName", "opacity")
        animate3.setAttribute ("values", "0;1;0")
        animate3.setAttribute ("dur", sprintf "%fs" snowFlake.Duration)
        animate3.setAttribute ("begin", sprintf "%fs" begin')
        animate3.setAttribute ("fill", "freeze")

        text.appendChild (animate1) |> ignore
        text.appendChild (animate2) |> ignore
        text.appendChild (animate3) |> ignore
        text

    [<Emit("setTimeout($0, $1)")>]
    let setTimeout (functionRef: unit -> unit) (delay: int) : int = jsNative

    [<Emit("clearInterval($0)")>]
    let clearInterval (intervalID: int) : unit = jsNative

    let randBetween min max =
        let rand = System.Random()
        rand.Next(min, max) |> float

    let rec fall (timeDOMLoaded: System.DateTime) (cease: bool) : unit =
        let snowArea = document.getElementById "snowArea"

        let startX =
            randBetween 0 (int (snowArea.getBoundingClientRect().width + 20. - 10.))

        let startY = randBetween 0 100

        let stopX = startX + ((randBetween 0 100) - 50.)

        let stopY =
            randBetween
                (int (snowArea.getBoundingClientRect().height / 2.))
                (int (snowArea.getBoundingClientRect().height))

        let fontSize = randBetween 20 120 |> int

        let fill =
            List.item (int (randBetween 0 5)) [ "#cccccc"; "#83d2df"; "#8de2f0"; "#83dfcd"; "#83b0df"; "#9683df" ]

        let snowFlake =
            create' { X = startX; Y = startY } { X = stopX; Y = stopY } fill fontSize

        let snowFlakeSvg =
            toElement snowFlake ((System.DateTime.Now - timeDOMLoaded).TotalSeconds |> float)

        if randBetween 0 9 < 4. then
            snowFlakeSvg.classList.add ("italic")

        snowArea.appendChild (snowFlakeSvg) |> ignore

        setTimeout
            (fun _ ->
                snowArea.removeChild snowFlakeSvg |> ignore

                let shouldStop =
                    (document.getElementById "umbrella").classList.contains "display-none"

                printfn "should stop snow: %b" shouldStop

                if shouldStop then
                    printfn "stopping snow."
                    ()
                else
                    printfn "falling snow."
                    fall timeDOMLoaded shouldStop)
            (int (snowFlake.Duration * 1.1 * 1000.))
        |> ignore
