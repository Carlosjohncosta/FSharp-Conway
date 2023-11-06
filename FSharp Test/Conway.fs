
module Conway
open System
open System.Threading

type Conway private (width: int, height: int, grid: bool[][]) =
    do
        Console.CursorVisible <- false

    static let ClampRange pos max =
        let fClamp = Math.Clamp(pos - 1, 0, max)
        let lClamp = Math.Clamp(pos + 1, 0, max)
        fClamp, lClamp

    let CheckNeighbors x y =
        let (xStart, xEnd) = ClampRange x (width - 1)
        let (yStart, yEnd) = ClampRange y (height - 1)
        (0, grid[xStart..xEnd])
        ||> Array.fold(fun acc row ->
            let rowCount = 
                (0, row[yStart..yEnd])
                ||> Array.fold(fun acc cell -> 
                    if cell then acc + 1 else acc)
            acc + rowCount)

    static member Random width height prob = 
        let rnd = Random()
        let grid = 
            [|for _ in 0..width -> 
                [|for _ in 0..height -> (rnd.Next() % prob) = 0|]|]
        Conway(width, height, grid)

    member this.Next =
        let newGrid =
            grid
            |> Array.mapi(fun x row ->
                row
                |> Array.mapi(fun y cell ->
                    let neighbors = CheckNeighbors x y
                    if 
                        cell then 5 > neighbors && neighbors > 2
                    else 
                        neighbors = 3))
        Conway(width, height, newGrid)

    member this.Print =
        grid
        |> Array.iteri(fun x row ->
            row
            |> Array.iteri(fun y cell ->
                Console.SetCursorPosition(x * 2, y)
                let color = 
                    if cell then 
                        ConsoleColor.Green 
                    else 
                        ConsoleColor.Black
                Console.BackgroundColor <- color
                printf "  "))


let rec main(next: Conway) =
    next.Print
    Thread.Sleep(10)
    main(next.Next)

main(Conway.Random 40 30 5)