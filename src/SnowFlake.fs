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
        text.setAttribute ("x", string snowFlake.StartPoint.X)
        text.setAttribute ("y", string snowFlake.StartPoint.Y)
        text.setAttribute ("font-size", sprintf "%dpx" snowFlake.FontSize)
        text.setAttribute ("font-family", "Noto Sans JP")
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
        animate3.setAttribute ("values", "1;0")
        animate3.setAttribute ("dur", sprintf "%fs" snowFlake.Duration)
        animate3.setAttribute ("begin", sprintf "%fs" begin')
        animate3.setAttribute ("fill", "freeze")

        text.appendChild (animate1) |> ignore
        text.appendChild (animate2) |> ignore
        text.appendChild (animate3) |> ignore
        text

    let toElement' (snowFlake: SnowFlake) (begin': float) : string =
        let ani1 =
            sprintf
                """<animate attributeName="%s" calcMode="%s" values="%s" dur="%fs" begin="%fs" fill="freeze" />"""
                "x"
                "linear"
                (sprintf "%f;%f" snowFlake.StartPoint.X snowFlake.StopPoint.X)
                snowFlake.Duration
                begin'

        let ani2 =
            sprintf
                """<animate attributeName="%s" calcMode="%s" values="%s" dur="%fs" begin="%fs" fill="freeze" />"""
                "y"
                "linear"
                (sprintf "%f;%f" snowFlake.StartPoint.Y snowFlake.StopPoint.Y)
                snowFlake.Duration
                begin'

        let ani3 =
            sprintf
                """<animate attributeName="%s" calcMode="%s" values="%s" dur="%fs" begin="%fs" fill="freeze" />"""
                "opacity"
                "linear"
                "1;0"
                snowFlake.Duration
                begin'

        let text =
            sprintf
                """<text x="%f" y="%f" stroke="none" font-family="Noto Sans JP" font-size="%dpx" fill="%s">*%s%s%s</text>"""
                snowFlake.StartPoint.X
                snowFlake.StartPoint.Y
                snowFlake.FontSize
                snowFlake.Fill
                ani1
                ani2
                ani3

        text

    [<Emit("setTimeout($0, $1)")>]
    let setTimeout (functionRef: unit -> unit) (delay: int) : int = jsNative

    [<Emit("clearInterval($0)")>]
    let clearInterval (intervalID: int) : unit = jsNative

    let randBetween min max =
        let rand = System.Random()
        rand.Next(min, max) |> float

    let rec fall acc : unit =
        let snowArea = document.getElementById "snowArea"

        let startX = randBetween 20 (int (snowArea.getBoundingClientRect().width - 20.))
        let startY = randBetween 20 100

        let stopX = startX + ((randBetween 0 20) - 10.)
        let stopY = randBetween 200 (int (snowArea.getBoundingClientRect().height))

        let fontSize = randBetween 40 120 |> int

        let snowFlake =
            create' { X = startX; Y = startY } { X = stopX; Y = stopY } "#aaccff" fontSize

        let snowFlakeSvg = toElement snowFlake acc
        snowArea.appendChild (snowFlakeSvg) |> ignore

        setTimeout
            (fun _ ->
                snowArea.removeChild (snowFlakeSvg) |> ignore
                fall (acc + snowFlake.Duration))
            (int (snowFlake.Duration * 1000.))
        |> ignore
