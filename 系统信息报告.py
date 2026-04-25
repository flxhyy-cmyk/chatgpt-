import platform, socket, datetime, webbrowser, os, subprocess
from pathlib import Path

try:
    import psutil
except ImportError:
    subprocess.check_call(["pip", "install", "psutil", "-q"])
    import psutil

# ── 数据采集 ──────────────────────────────────────────────

def get_size(b):
    for u in ["B","KB","MB","GB","TB"]:
        if b < 1024:
            return f"{b:.2f} {u}"
        b /= 1024

now = datetime.datetime.now().strftime("%Y-%m-%d %H:%M:%S")
boot = datetime.datetime.fromtimestamp(psutil.boot_time())
uptime = datetime.datetime.now() - boot
uptime_str = f"{int(uptime.total_seconds()//3600)} 小时 {int((uptime.total_seconds()%3600)//60)} 分钟"

# 系统
sys_info = {
    "操作系统": f"{platform.system()} {platform.release()}",
    "系统版本": platform.version()[:60] + "...",
    "主机名": socket.gethostname(),
    "当前用户": os.getlogin() if hasattr(os, 'getlogin') else platform.node(),
    "系统架构": platform.machine(),
    "Python版本": platform.python_version(),
    "开机时间": boot.strftime("%Y-%m-%d %H:%M:%S"),
    "已运行时长": uptime_str,
}

# CPU
cpu = psutil.cpu_freq()
cpu_info = {
    "处理器": platform.processor() or "未知",
    "物理核心数": psutil.cpu_count(logical=False),
    "逻辑核心数": psutil.cpu_count(logical=True),
    "当前频率": f"{cpu.current:.0f} MHz" if cpu else "未知",
    "最大频率": f"{cpu.max:.0f} MHz" if cpu else "未知",
    "CPU使用率": f"{psutil.cpu_percent(interval=1)} %",
}

# 内存
mem = psutil.virtual_memory()
swap = psutil.swap_memory()
mem_info = {
    "总内存": get_size(mem.total),
    "已使用": get_size(mem.used),
    "可用内存": get_size(mem.available),
    "内存使用率": f"{mem.percent} %",
    "交换区总量": get_size(swap.total),
    "交换区已用": get_size(swap.used),
}

# 磁盘
disk_rows = ""
for part in psutil.disk_partitions():
    try:
        usage = psutil.disk_usage(part.mountpoint)
        pct = usage.percent
        color = "#e74c3c" if pct > 85 else "#f39c12" if pct > 60 else "#2ecc71"
        disk_rows += f"""
        <tr>
            <td>{part.device}</td>
            <td>{part.mountpoint}</td>
            <td>{part.fstype}</td>
            <td>{get_size(usage.total)}</td>
            <td>{get_size(usage.used)}</td>
            <td>{get_size(usage.free)}</td>
            <td>
                <div style="background:#eee;border-radius:4px;height:16px;width:120px;display:inline-block;vertical-align:middle">
                    <div style="background:{color};width:{pct}%;height:100%;border-radius:4px"></div>
                </div>
                <span style="color:{color};font-weight:bold"> {pct}%</span>
            </td>
        </tr>"""
    except:
        pass

# 网络
net_rows = ""
for name, addrs in psutil.net_if_addrs().items():
    for addr in addrs:
        if addr.family == socket.AF_INET:
            net_rows += f"<tr><td>{name}</td><td>{addr.address}</td><td>{addr.netmask}</td></tr>"

# 进程 Top10
procs = sorted(psutil.process_iter(['pid','name','cpu_percent','memory_percent','status']),
               key=lambda p: p.info['memory_percent'] or 0, reverse=True)[:10]
proc_rows = ""
for p in procs:
    i = p.info
    proc_rows += f"<tr><td>{i['pid']}</td><td>{i['name']}</td><td>{i['cpu_percent']:.1f}%</td><td>{i['memory_percent']:.1f}%</td><td>{i['status']}</td></tr>"

# ── HTML 报告 ──────────────────────────────────────────────

def section(title, icon, content):
    return f"""
    <div class="card">
        <h2>{icon} {title}</h2>
        {content}
    </div>"""

def kv_table(data):
    rows = "".join(f"<tr><th>{k}</th><td>{v}</td></tr>" for k,v in data.items())
    return f"<table class='kv'>{rows}</table>"

cpu_pct = float(psutil.cpu_percent(interval=0))
mem_pct = mem.percent
cpu_color = "#e74c3c" if cpu_pct>80 else "#f39c12" if cpu_pct>50 else "#2ecc71"
mem_color = "#e74c3c" if mem_pct>80 else "#f39c12" if mem_pct>50 else "#2ecc71"

html = f"""<!DOCTYPE html>
<html lang="zh">
<head>
<meta charset="UTF-8">
<title>系统信息报告</title>
<style>
  * {{ box-sizing:border-box; margin:0; padding:0 }}
  body {{ font-family:'Microsoft YaHei',Arial,sans-serif; background:#f0f2f5; color:#333 }}
  header {{ background:linear-gradient(135deg,#1a1a2e,#16213e,#0f3460); color:#fff; padding:32px 40px }}
  header h1 {{ font-size:28px; margin-bottom:6px }}
  header p {{ opacity:.7; font-size:14px }}
  .container {{ max-width:1000px; margin:30px auto; padding:0 20px 40px }}
  .summary {{ display:flex; gap:16px; margin-bottom:24px; flex-wrap:wrap }}
  .stat {{ flex:1; min-width:140px; background:#fff; border-radius:12px; padding:20px; text-align:center; box-shadow:0 2px 8px rgba(0,0,0,.08) }}
  .stat .val {{ font-size:32px; font-weight:bold; margin:8px 0 }}
  .stat .lbl {{ font-size:12px; color:#888 }}
  .card {{ background:#fff; border-radius:12px; padding:24px; margin-bottom:20px; box-shadow:0 2px 8px rgba(0,0,0,.08) }}
  .card h2 {{ font-size:17px; margin-bottom:16px; padding-bottom:10px; border-bottom:2px solid #f0f2f5 }}
  table {{ width:100%; border-collapse:collapse; font-size:14px }}
  table th, table td {{ padding:10px 14px; text-align:left; border-bottom:1px solid #f0f2f5 }}
  table.kv th {{ width:160px; color:#888; font-weight:normal; background:#fafafa }}
  table tr:last-child th, table tr:last-child td {{ border-bottom:none }}
  thead th {{ background:#f8f9fa; font-weight:600; color:#555 }}
  footer {{ text-align:center; color:#aaa; font-size:12px; margin-top:20px }}
</style>
</head>
<body>
<header>
  <h1>🖥️ 系统信息报告</h1>
  <p>生成时间：{now} &nbsp;|&nbsp; 主机：{socket.gethostname()}</p>
</header>
<div class="container">
  <div class="summary">
    <div class="stat"><div class="lbl">CPU 使用率</div><div class="val" style="color:{cpu_color}">{cpu_pct:.0f}%</div></div>
    <div class="stat"><div class="lbl">内存使用率</div><div class="val" style="color:{mem_color}">{mem_pct:.0f}%</div></div>
    <div class="stat"><div class="lbl">总内存</div><div class="val" style="color:#3498db">{get_size(mem.total)}</div></div>
    <div class="stat"><div class="lbl">逻辑 CPU 核心</div><div class="val" style="color:#9b59b6">{psutil.cpu_count()}</div></div>
    <div class="stat"><div class="lbl">已运行时长</div><div class="val" style="font-size:18px;color:#1abc9c">{uptime_str}</div></div>
  </div>

  {section("系统基本信息", "🔷", kv_table(sys_info))}
  {section("CPU 信息", "⚙️", kv_table(cpu_info))}
  {section("内存信息", "🧠", kv_table(mem_info))}

  {section("磁盘信息", "💾", f"""
  <table>
    <thead><tr><th>设备</th><th>挂载点</th><th>文件系统</th><th>总容量</th><th>已使用</th><th>可用</th><th>使用率</th></tr></thead>
    <tbody>{disk_rows}</tbody>
  </table>""")}

  {section("网络信息", "🌐", f"""
  <table>
    <thead><tr><th>网卡</th><th>IP 地址</th><th>子网掩码</th></tr></thead>
    <tbody>{net_rows}</tbody>
  </table>""")}

  {section("进程 Top 10（按内存占用）", "🔥", f"""
  <table>
    <thead><tr><th>PID</th><th>进程名</th><th>CPU%</th><th>内存%</th><th>状态</th></tr></thead>
    <tbody>{proc_rows}</tbody>
  </table>""")}

  <footer>由 Claude AI 生成 · {now}</footer>
</div>
</body>
</html>"""

out = Path.home() / "系统信息报告.html"
out.write_text(html, encoding="utf-8")
print(f"✅ 报告已生成：{out}")
webbrowser.open(out.as_uri())
