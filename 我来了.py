import tkinter as tk

root = tk.Tk()
root.title("消息")
root.geometry("300x150")
root.resizable(False, False)

# 居中窗口
root.eval('tk::PlaceWindow . center')

label = tk.Label(root, text="我来了", font=("Arial", 36, "bold"), fg="#333333")
label.pack(expand=True)

btn = tk.Button(root, text="确定", command=root.destroy, width=10, font=("Arial", 12))
btn.pack(pady=10)

root.mainloop()
