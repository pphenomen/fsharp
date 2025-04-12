open System
open System.Windows.Forms

let form = new Form(Text = "WinForm", Width = 300, Height = 400)

let label = new Label(Top = 10, Left = 20, Width = 250, Height = 40, Text = "Массив из первых 100 натуральных чисел,\nкратных 13 или 17:")
let listBox = new ListBox(Top = 50, Left = 20, Width = 240, Height = 300)
form.Controls.Add(label)
form.Controls.Add(listBox)

let array =
    Seq.initInfinite (fun num -> num + 1) 
    |> Seq.filter (fun x -> x % 13 = 0 || x % 17 = 0)
    |> Seq.truncate 100
    |> Seq.toArray

for n in array do
    listBox.Items.Add(n)

[<STAThread>]
[<EntryPoint>]
let main _ =
    Application.Run(form)
    0
