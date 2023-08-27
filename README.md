# VOICEPEAK binding for Emacs

Let's speak with [VOICEPEAK] from Emacs.

```el
(progn
  (let ((voicepeak-narrator "Zundamon"))
    (voicepeak-say "ずんだもんなのだ。"))
  (let ((voicepeak-narrator "Tohoku Zunko"))
    (voicepeak-say "こんにちは、東北ずん子です。"))
  (let ((voicepeak-narrator "Jashinchan"))
    (voicepeak-say "邪神ちゃんドロップキーック！")))
```

> **Warning**
> We have not confirmed its operation in the free trial version.

## Copyright

`voicepeak.el` licensed under [GNU General Public License Version 3][gpl-v3] (GPLv3).

> ```
> Copyright (C) 2023  USAMI Kenta <tadsan@zonu.me>
> ```
>
> This program is free software; you can redistribute it and/or modify
> it under the terms of the GNU General Public License as published by
> the Free Software Foundation, either version 3 of the License, or
> (at your option) any later version.
>
> This program is distributed in the hope that it will be useful,
> but WITHOUT ANY WARRANTY; without even the implied warranty of
> MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
> GNU General Public License for more details.
>
> You should have received a copy of the GNU General Public License
> along with this program.  If not, see <https://www.gnu.org/licenses/>.

> **Note**
> The licensing scope of the generated audio files varies depending on the source.<br>Please check [the personal use permission range](https://www.ah-soft.com/commercial/voicepeak/private/) page.

[VOICEPEAK]: https://ja.wikipedia.org/wiki/VOICEPEAK
[gpl-v3]: https://www.gnu.org/licenses/quick-guide-gplv3.html
