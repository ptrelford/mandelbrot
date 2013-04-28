namespace Mandelbrot

[<AutoOpen>]
module Computation =
    open System
    open System.Numerics

    let maxIteration = 512

    let (|Escaped|DidNotEscape|) (cx,cy) =
        let rec compute (zx,zy) iteration =
            if iteration = maxIteration then DidNotEscape
            elif zx * zx + zy * zy > 4.0 then Escaped iteration
            else compute (zx*zx - zy*zy + cx, 2.0*zx * zy + cy) (iteration+1)
        compute (cx,cy) 0 

module RGB =
    let FromHSL(H, S, L) =      
      let v = 
        if (L <= 0.5) 
        then (L * (1.0 + S)) 
        else (L + S - L * S)
      if v > 0.0 then                
        let m = L + L - v;
        let sv = (v - m) / v;
        let H = H * 6.0;
        let sextant = int H
        let fract = H - float sextant
        let vsf = v * sv * fract;
        let mid1 = m + vsf;
        let mid2 = v - vsf;
        match sextant with
        | 0 -> v, mid1, m
        | 1 -> mid2, v, m
        | 2 -> m, v, mid1
        | 3 -> m, mid2, v
        | 4 -> mid1, m, v
        | 5 -> v, m, mid2
        | _ -> L, L, L
      else L,L,L
      |> fun (r,g,b) -> int(r*255.),int(g*255.),int(b*255.)

open System.Numerics
open System.Windows
open System.Windows.Controls
open System.Windows.Media
open System.Windows.Media.Effects
open System.Windows.Media.Imaging
open Utils

type ZoomControl(width:int,height:int) as control =
    inherit UserControl()
    let bitmap = WriteableBitmap(width,height)
    let canvas = Canvas()
    do  canvas.Children.Add(Image(Source=bitmap))
    do  control.Content <- canvas 

    let colors =
        [|for i in 0..maxIteration ->
            let n = float i / float maxIteration
            let H,S,L = 0.95 + (10.0 * n), 0.6, 0.5
            let H = H - floor H
            let r,g,b = RGB.FromHSL(H,S,L)
            0xff000000 + (r <<< 16) + (g <<< 8) + b|]

    let render (x1,y1,x2,y2) (offset, height, stripe) (pixels:int[]) =
        let dx, dy = x2-x1, y2-y1
        for y = 0 to height-1 do
            for x = 0 to width-1 do
                let y = offset + (y*stripe)
                let n = x + y*width
                pixels.[n] <-
                    let x = ((float x/float width) * dx) + x1
                    let y = ((float y/float (height*stripe)) * dy) + y1 
                    match (x, y) with
                    | DidNotEscape -> 0xff000000
                    | Escaped i -> colors.[i]                    

    let mutable points = (-2.0, -1.0, 1.0, 1.0)
    do  render points (0,height,1) bitmap.Pixels

    let copy (l,t,w,h) =
        let selection = WriteableBitmap(int w, int h)
        let source = bitmap.Pixels
        let dest = selection.Pixels
        for y = 0 to int h - 1 do
            for x = 0 to int w - 1 do
                dest.[x + (y * int w)] <- 
                    source.[int l + x + ((int t + y) * width)]
        selection

    let moveControl (element:FrameworkElement) (start:Point) (finish:Point) =
        element.Width <- abs(finish.X - start.X)
        element.Height <- abs(finish.Y - start.Y)
        Canvas.SetLeft(element, min start.X finish.X)
        Canvas.SetTop(element, min start.Y finish.Y)

    let transparentGray = 
        SolidColorBrush(Color.FromArgb(128uy, 164uy, 164uy, 164uy))

    let buffer = Array.create (width * height) 0

    let rec waiting() = async {
        let! md = Async.AwaitObservable(control.MouseLeftButtonDown)
        let rc = new Canvas(Background = transparentGray)
        canvas.Children.Add(rc) 
        do! drawing(rc, md.GetPosition(canvas)) }

    and drawing(rc:Canvas, pos) = async {
        let! evt = Async.AwaitObservable(canvas.MouseLeftButtonUp, canvas.MouseMove)
        match evt with
        | Choice1Of2(up) ->
            let l, t = Canvas.GetLeft(rc), Canvas.GetTop(rc)
            let w, h = rc.Width, rc.Height
            if w > 1.0 && h > 1.0 then
                let preview = 
                    Image(Source=copy (l,t,w,h),
                          Stretch=Stretch.Fill,
                          Width=float width,Height=float height)
                canvas.Children.Add preview

                let zoom (x1,y1,x2,y2) =           
                    let tx x = ((x/float width) * (x2-x1)) + x1
                    let ty y = ((y/float height) * (y2-y1)) + y1
                    tx l, ty t, tx (l+w), ty (t+h)

                points <- zoom points
                
                let start = System.Environment.TickCount
                let threads = System.Environment.ProcessorCount                
                do! [0..threads-1] 
                    |> List.map (fun y -> async {
                        render points (y,(height/threads),threads) bitmap.Pixels
                    })
                    |> Async.Parallel
                    |> Async.Ignore
                let finish = System.Environment.TickCount
                let elapsed = finish - start
                canvas.Children.Remove preview |> ignore

                let text = TextBlock(Text=sprintf "Time: %dms" elapsed)
                text.Foreground <- SolidColorBrush Colors.White
                text.Effect <- DropShadowEffect(Color=Colors.Black)
                canvas.Children.Add text
                do! async {
                        do! Async.Sleep 2000
                        canvas.Dispatcher.BeginInvoke(fun () ->   
                            canvas.Children.Remove text |> ignore
                        ) |> ignore
                    }
                    |> Async.StartChild
                    |> Async.Ignore
                
            canvas.Children.Remove rc |> ignore
            do! waiting() 
        | Choice2Of2(move) ->
            moveControl rc pos (move.GetPosition(canvas))
            do! drawing(rc, pos) }
    
    do  waiting() |> Async.StartImmediate

type App() as app =
    inherit System.Windows.Application()
    do  app.Startup.Add(fun _ -> app.RootVisual <- ZoomControl(512,384))