open System
open System.Windows.Forms

let form = new Form(Text = "List", Width = 400, Height = 200)

let label1 = new Label(Text = "Введите список:", Left = 20,Top = 10, Width = 100, Height = 20)
let inputBox = new TextBox(Top = 30, Left = 20, Width = 350)
let mirrorButton = new Button(Text = "Отзеркалить", Top = 65, Left = 150, Width = 100, Height = 30)
let label2 = new Label(Text = "Результат:", Left = 20, Top = 100, Width = 100, Height = 20)
let label3 = new Label(Top = 120, Left = 20, Width = 350, Height = 50)

form.Controls.Add(label1)
form.Controls.Add(inputBox)
form.Controls.Add(mirrorButton)
form.Controls.Add(label2)
form.Controls.Add(label3)

mirrorButton.Click.Add(fun _ ->
    let text = inputBox.Text.Split([|','; ';'; ' '|]) |> Array.map (fun str -> str.Trim()) |> Array.rev |> String.concat " "
    label3.Text <- text
)

[<STAThread>]
[<EntryPoint>]
let main _ =
    Application.Run(form)
    0
