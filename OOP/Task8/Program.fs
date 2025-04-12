open System
open System.Windows.Forms

let form = new Form(Text = "Тригонометрия", Width = 300, Height = 200)
let input = new TextBox(Top = 20, Left = 30, Width = 200)
let result = new Label(Top = 60, Left = 30, Width = 200, Height = 25, Text = "Результат")

let cosButton = new Button(Text = "cos(x)", Top = 100, Left = 30, Width = 70)
let sinButton = new Button(Text = "sin(x)", Top = 100, Left = 110, Width = 70)
let tanButton = new Button(Text = "tan(x)", Top = 100, Left = 190, Width = 70)

let compute trigFunc =
    try
        let x = float input.Text
        let radians = x * System.Math.PI / 180.0
        let value = trigFunc radians
        result.Text <- sprintf "Результат: %.4f" value
    with _ ->
        result.Text <- "Ошибка ввода"

cosButton.Click.Add(fun _ -> compute Math.Cos)
sinButton.Click.Add(fun _ -> compute Math.Sin)
tanButton.Click.Add(fun _ -> compute Math.Tan)

form.Controls.Add(input)
form.Controls.Add(result)
form.Controls.Add(cosButton)
form.Controls.Add(sinButton)
form.Controls.Add(tanButton)

[<STAThread>]
[<EntryPoint>]
let main _ =
    Application.Run(form)
    0