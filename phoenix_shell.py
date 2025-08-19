import os
import time
import subprocess

GLYPH_FRAMES = [
    "⟡", "⟠", "⟣", "⟢", "⟡"
]

def animate_glyph():
    for frame in GLYPH_FRAMES:
        print(f"\r🔥 {frame} Compiling...", end="", flush=True)
        time.sleep(0.2)
    print("\r🔥 ⟠ Compilation complete.        ")

def run_capsule(path):
    print("🛡️ Guardian shell activated.")
    animate_glyph()
    result = subprocess.run([
        "Phoenix_ProLang_Compiler.exe", path
    ], cwd="dist", capture_output=True, text=True)
    print("🧾 Output:")
    print(result.stdout)
    if result.stderr:
        print("⚠️ Errors:")
        print(result.stderr)

if __name__ == "__main__":
    capsule_path = "../Guardian.phx"
    run_capsule(capsule_path)
