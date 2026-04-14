# parakeet.cpp Setup (macOS)

Setup guide for [parakeet.cpp](https://github.com/Frikallo/parakeet.cpp), NVIDIA's FastConformer-based transcription engine. Achieves lower word error rates than Whisper at equivalent model sizes, with dramatically faster inference on Apple Silicon via Metal GPU acceleration.

## Why Parakeet?

| Engine | Model | Params | LibriSpeech Clean WER | Speed (10s audio, M-series GPU) |
|--------|-------|--------|----------------------|--------------------------------|
| Whisper | small (default) | 244M | 3.4% | ~200ms |
| Whisper | medium | 769M | 2.9% | ~600ms |
| Whisper | large-v3 | 1.55B | 2.4% | ~1.5s |
| **Parakeet** | **tdt-ctc-110m** | **110M** | **2.4%** | **~27ms** |
| **Parakeet** | **tdt-600m** | **600M** | **1.7%** | **~520ms** |

Parakeet's 110M model matches Whisper large-v3 accuracy at 14x fewer parameters.

## Supported Models

| Model | Languages | Size |
|-------|-----------|------|
| `tdt-ctc-110m` | English only | ~220 MB |
| `tdt-600m` | 25 European languages | ~1.2 GB |

25 languages: Bulgarian, Croatian, Czech, Danish, Dutch, English, Estonian, Finnish, French, German, Greek, Hungarian, Italian, Latvian, Lithuanian, Maltese, Polish, Portuguese, Romanian, Russian, Slovak, Slovenian, Spanish, Swedish, Ukrainian. For anything outside this list, use Whisper (99 languages).

## Prerequisites

Metal GPU support requires the **full Xcode app** — the `metal` shader compiler is not included in the standalone Command Line Tools package.

```bash
# 1. Install Xcode from the App Store (or: mas install 497799835)

# 2. Accept the license
sudo xcodebuild -license accept

# 3. Point the developer directory at Xcode (not just CLT)
sudo xcode-select -s /Applications/Xcode.app/Contents/Developer

# 4. Download the Metal Toolchain component
#    Even with Xcode installed, this component may be missing
xcodebuild -downloadComponent MetalToolchain
```

> **Common gotcha:** Xcode may install without the Metal Toolchain. The build will fail with:
> `cannot execute tool 'metal' due to missing Metal Toolchain; use: xcodebuild -downloadComponent MetalToolchain`
> Running step 4 above fixes it.

## Build

```bash
git clone --recursive https://github.com/Frikallo/parakeet.cpp
cd parakeet.cpp
make build
cp build/parakeet ~/.local/bin/
```

## Install Models

Models are distributed as `.nemo` files on HuggingFace and must be converted to safetensors format using `torch` and `safetensors`. Use `uv run` to avoid creating a persistent venv — it handles the deps inline and cleans up after itself. Note `torch` is ~2 GB so the first run will take a few minutes to download.

```bash
# Download the model (note: use `hf`, not `huggingface-cli` — the latter is deprecated)
hf download nvidia/parakeet-tdt_ctc-110m --include '*.nemo' --local-dir .

# Convert to safetensors using uv (no venv setup needed)
mkdir -p ~/.minutes/models/parakeet
uv run --with torch --with safetensors --with packaging --with numpy python scripts/convert_nemo.py *.nemo \
  -o ~/.minutes/models/parakeet/tdt-ctc-110m.safetensors

# Extract the vocab file from the .nemo archive (it's a tarball)
# Note: macOS BSD tar doesn't support --wildcards, so we resolve the filename first
tar xf *.nemo --strip-components=1 $(tar tf *.nemo | grep 'vocab\.txt')
cp *vocab.txt ~/.minutes/models/parakeet/vocab.txt
```

For the larger multilingual model (`tdt-600m`):

```bash
hf download nvidia/parakeet-tdt-0.6b-v2 --include '*.nemo' --local-dir .
mkdir -p ~/.minutes/models/parakeet/tdt-600m
uv run --with torch --with safetensors --with packaging --with numpy python scripts/convert_nemo.py parakeet-tdt-0.6b-v2.nemo \
  -o ~/.minutes/models/parakeet/tdt-600m/tdt-600m.safetensors \
  --model 600m-tdt

# Extract the SentencePiece tokenizer vocab (macOS BSD tar: resolve filename first)
tar xf parakeet-tdt-0.6b-v2.nemo --strip-components=1 $(tar tf parakeet-tdt-0.6b-v2.nemo | grep 'tokenizer\.vocab')
cp *tokenizer.vocab ~/.minutes/models/parakeet/tdt-600m/tdt-600m.tokenizer.vocab

# Convert Silero VAD weights
uv run --with torch --with safetensors --with packaging --with numpy --with torchaudio python scripts/convert_silero_vad.py \
  -o ~/.minutes/models/parakeet/silero_vad_v5.safetensors
```

If you have multiple models installed, keep each in its own directory with model-specific filenames (e.g. `tdt-600m/tdt-600m.tokenizer.vocab`) so model switches stay deterministic.

## Configure Minutes

```toml
# ~/.config/minutes/config.toml
[transcription]
engine = "parakeet"
parakeet_model = "tdt-600m"                                 # or "tdt-ctc-110m"
parakeet_binary = "/Users/you/.local/bin/parakeet"          # use absolute path
parakeet_vocab = "tdt-600m.tokenizer.vocab"
parakeet_boost_limit = 25                                   # experimental
parakeet_boost_score = 2.0                                  # experimental
parakeet_fp16 = true                                        # experimental
```

> **macOS desktop app note:** Apps launched from Finder, Spotlight, or the Dock do not inherit your shell `PATH`. Always use an **absolute path** for `parakeet_binary`. Common locations:
> - `/opt/homebrew/bin/parakeet`
> - `/usr/local/bin/parakeet`
> - `/Users/you/.local/bin/parakeet`

To switch back to Whisper: set `engine = "whisper"`. No rebuild needed.

## Troubleshooting

### Metal shader compiler not found (build fails at ~19%)

```
xcrun: error: unable to find utility "metal", not a developer tool or in PATH
```

You have Command Line Tools but not the full Xcode app.

```bash
sudo xcode-select -s /Applications/Xcode.app/Contents/Developer
```

### Metal Toolchain missing (build fails after Xcode install)

```
error: cannot execute tool 'metal' due to missing Metal Toolchain
```

Xcode is installed but the Metal Toolchain component is absent.

```bash
xcodebuild -downloadComponent MetalToolchain
```

### "parakeet binary not found"

The binary isn't on the PATH the app sees. Set an absolute path in config:

```toml
parakeet_binary = "/Users/you/.local/bin/parakeet"
```

### "unknown parakeet model"

Only `tdt-ctc-110m` and `tdt-600m` are valid values for `parakeet_model`.

### "Expected parakeet model in ~/.minutes/models/parakeet/"

Run `minutes setup --parakeet` or follow the manual model install steps above.

### CMake 4.x atomics error

Google Highway's `FindAtomics.cmake` is incompatible with CMake 4.x on Apple Silicon. The check forces `CMAKE_CXX_STANDARD 11` which conflicts with the project's C++20 requirement. Workaround: patch the check to short-circuit on `APPLE AND arm64`.
