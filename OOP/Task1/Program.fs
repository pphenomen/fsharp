open System

type IPrint = interface
    abstract member Print: unit -> unit
    end

[<AbstractClass>]
type Figure() =
    abstract member Area: unit -> float

type Rectangle(width: float, height: float) =
    inherit Figure()

    member this.Width = width
    member this.Height = height

    override this.Area() = this.Width * this.Height
    override this.ToString() = sprintf "Прямоугольник: [ширина: %f, высота: %f, площадь: %f]" this.Width this.Height (this.Area())

    interface IPrint with
        member this.Print() =
            Console.WriteLine(this.ToString())

type Square(side: float) = 
    inherit Rectangle(side, side)
    
    member this.Side = side

    override this.ToString() = sprintf "Квадрат: [длина стороны: %f, площадь: %f]" this.Side (this.Area())

    interface IPrint with
        member this.Print() =
            Console.WriteLine(this.ToString())

type Circle(radius: float) =
    inherit Figure()

    member this.Radius = radius

    override this.Area() = System.Math.PI * radius * radius
    override this.ToString() = sprintf "Круг: [радиус: %f, площадь: %f]" this.Radius (this.Area())

    interface IPrint with
        member this.Print() =
            Console.WriteLine(this.ToString())

let s = Circle(3.)
Console.WriteLine(s)
    