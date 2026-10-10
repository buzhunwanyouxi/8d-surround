$nyquist plug-in
$version 4
$type generate
$name (_ "8D 测试音 (立体声)")
$debugbutton disabled
$author (_ "8D环绕穿梭")
$release 2.2.0
$copyright (_ "GNU General Public License v2.0 or later")

;; =====================================================================
;; 8D 测试信号生成器
;; =====================================================================
;; 用法: 生成 -> 8D 测试音 (立体声)      (不需要选区, 一键出结果)
;;
;; 输出 16 秒, 分两段:
;;   [0-4 秒]  左右交替开关注 (每 1 秒一格)
;;             左耳: 0.7-0.9 / 0.3-0.5
;;             右耳: 0.9-1.1 / 0.5-0.7
;;             波形上上下两条明显交叉, 极易辨认
;;   [4-16 秒] 1000 Hz 正弦, 左右强弱交替, 6 秒一个来回
;;
;; ---------------------------------------------------------------------
;; 2.2.0 修正: osc 的第 1 个参数是"音级(step)", 不是 Hz!
;;
;;   旧版写 (osc 1000.0 ...) —— step-to-hz(1000) 高到 8.9e11 Hz,
;;   触发 osc 内部的防混叠保护 "frequency reduced by N octaves ...
;;   to avoid aliasing", 实际出来的不是 1000 Hz, 而是接近奈奎斯特
;;   频率的杂音 (Audacity 日志里能直接看到这条 warning)。
;;
;;   旧版 (osc (/ 1.0 6.0) ...) —— 被当成 step 0.1667 = 8.26 Hz,
;;   而不是设计要的 1/6 = 0.1667 Hz。
;;
;;   正确写法:
;;     想要某个 Hz   -> (osc (hz-to-step 1000.0) ...)
;;     只想要慢包络  -> 用 lfo, 它的第 1 个参数本来就是 Hz
;;   (lfo 还有两个好处: 输出正好 ±1, 不会被 *loud* 缩放。)
;; =====================================================================


;; ---------- 基础素材 ----------
;; 1 秒静音
(setf s8d-r1 (s-rest 1.0))

;; 1000 Hz 正弦, 1 秒      (osc 要传音级, 用 hz-to-step 换算)
(setf s8d-t1 (abs-env (osc (hz-to-step 1000.0) 1.0 *sine-table* 0)))
;; 1000 Hz 正弦, 2 秒      (备用)
(setf s8d-t2 (abs-env (osc (hz-to-step 1000.0) 2.0 *sine-table* 0)))
;; 1000 Hz 正弦, 12 秒     (音乐段主体)
(setf s8d-t12 (abs-env (osc (hz-to-step 1000.0) 12.0 *sine-table* 0)))

;; 左耳开关注: [静][响][静][响]
(setf s8d-tag-l
  (seq s8d-r1
       (mult 0.7 s8d-t1)
       s8d-r1
       (mult 0.7 s8d-t1)))

;; 右耳开关注: [响][静][响][静]
(setf s8d-tag-r
  (seq (mult 0.7 s8d-t1)
       s8d-r1
       (mult 0.7 s8d-t1)
       s8d-r1))


;; ---------- 音乐段 (6 秒一个来回) ----------
;; 慢速正弦 b: 幅度 ±1, 周期 6 秒, 时长 12 秒  -> 恰好两个完整周期
(setf s8d-b (abs-env (lfo (/ 1.0 6.0) 12.0 *sine-table* 0)))
;; 左耳包络 = 0.5 + 0.5*b   -> 0 到 1 之间
(setf s8d-el (sum 0.5 (mult 0.5 s8d-b)))
;; 右耳包络 = 0.5 - 0.5*b   -> 与左耳互补
(setf s8d-er (sum 0.5 (mult -0.5 s8d-b)))
;; 叠加到正弦上, 保持总响度稳定, 最低不让它完全静音
(setf s8d-mus-l (mult 0.8 s8d-t12 (sum 0.3 (mult 0.7 s8d-el))))
(setf s8d-mus-r (mult 0.8 s8d-t12 (sum 0.3 (mult 0.7 s8d-er))))

;; ---------- 拼接: 0-4 秒标记段 + 4-16 秒音乐段 ----------
(vector (sim s8d-tag-l (at 4.0 s8d-mus-l))
        (sim s8d-tag-r (at 4.0 s8d-mus-r)))
