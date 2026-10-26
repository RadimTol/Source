#!/usr/bin/env python3
"""Offline English -> Czech translation via locally cached Helsinki-NLP model.
Usage: python translate_local.py input.json output.json
Model is downloaded from Hugging Face on the first run only.
"""
import json
import os
import sys

MODEL = os.environ.get('LOCAL_TRANSLATION_MODEL', 'Helsinki-NLP/opus-mt-en-cs')


def main():
    if len(sys.argv) != 3:
        raise SystemExit('Usage: python translate_local.py input.json output.json')
    with open(sys.argv[1], encoding='utf-8') as f:
        texts = json.load(f)
    if not isinstance(texts, list) or any(not isinstance(x, str) for x in texts):
        raise ValueError('Input must be a JSON array of strings')
    if not texts:
        with open(sys.argv[2], 'w', encoding='utf-8') as f:
            json.dump([], f)
        return

    import torch
    from transformers import MarianMTModel, MarianTokenizer

    local_only = os.environ.get('LOCAL_TRANSLATION_OFFLINE', '0') == '1'
    tokenizer = MarianTokenizer.from_pretrained(MODEL, local_files_only=local_only)
    model = MarianMTModel.from_pretrained(MODEL, local_files_only=local_only)
    model.eval()
    # Work on CPU to remain usable on ordinary Windows PCs without GPU.
    model.to('cpu')
    translated = []
    with torch.inference_mode():
        for text in texts:
            if not text.strip():
                translated.append('')
                continue
            # Translate by chunks of sentences to stay within the model's input limit.
            import re
            sentences = re.split(r'(?<=[.!?])\s+', text.strip())
            chunks = []
            chunk = ''
            for sentence in sentences:
                # Token length, not character length, controls the model limit.
                words = sentence.split()
                segments = []
                part = ''
                for word in words:
                    trial = (part + ' ' + word).strip()
                    if part and len(tokenizer.encode(trial, add_special_tokens=True)) > 350:
                        segments.append(part)
                        part = word
                    else:
                        part = trial
                if part:
                    segments.append(part)
                for segment in segments:
                    trial = (chunk + ' ' + segment).strip()
                    if chunk and len(tokenizer.encode(trial, add_special_tokens=True)) > 350:
                        chunks.append(chunk)
                        chunk = segment
                    else:
                        chunk = trial
            if chunk:
                chunks.append(chunk)
            pieces = []
            for piece in chunks:
                encoded = tokenizer(piece, return_tensors='pt', truncation=True, max_length=480)
                result = model.generate(**encoded, max_new_tokens=512, num_beams=4)
                pieces.append(tokenizer.decode(result[0], skip_special_tokens=True))
            translated.append(' '.join(pieces))
    with open(sys.argv[2], 'w', encoding='utf-8') as f:
        json.dump(translated, f, ensure_ascii=False)


if __name__ == '__main__':
    main()
