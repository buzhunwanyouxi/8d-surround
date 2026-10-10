$nyquist plug-in
$version 4
$type process
$preview linear
$name (_ "8D环绕穿梭")
$debugbutton disabled
$author (_ "8D环绕穿梭")
$release 2.2.0
$copyright (_ "GNU General Public License v2.0 or later")

;; =====================================================================
;; 8D 环绕穿梭 (8D Surround Pan)  --  Nyquist plug-in for Audacity 3.7
;; =====================================================================
;; 2.2.0 修正了 5 个致命/无效 bug。这些都是用 Audacity 自己的日志
;; (%APPDATA%\audacity\lastlog.txt) 和随软件附带的 Nyquist 源码
;; (D:\ruanjianpan\Audacity\Nyquist\*.lsp) 逐条核实过的, 不是猜的:
;;
;;  [致命 1] (snd-srate *track*)  ->  采样率必须用全局 *sound-srate*。
;;        官方文档原话: "all Nyquist snd-... low-level functions only
;;        can process mono signals"。立体声时 *track* 是"2 个 Sound 的
;;        数组", 于是 (snd-srate *track*) 立刻抛
;;            error: bad argument type - #(#<Sound> #<Sound>)
;;        整个效果中止, 音频原样返回 —— 这就是"波形完全没变化"的真因。
;;
;;  [致命 2] (osc (/ 1.0 period) ...)  ->  osc 的第 1 个参数是"音级
;;        (step)"不是 Hz。传 1/6 会被当成 step 0.1667, 实际频率
;;            step-to-hz(0.1667) = 8.257 Hz
;;        而且周期不管设 1 秒还是 60 秒, 结果都落在 8.19-8.66 Hz 之间
;;        (只差 5.7%) —— 周期旋钮等于完全失效。
;;        改用 lfo, 它的第 1 个参数就是 Hz (原版"穿脑效果"用的就是 lfo)。
;;
;;  [致命 3] (s-zeros ...)  ->  这个函数根本不存在。
;;        日志原话: error: unbound function - S-ZEROS
;;        整个 Nyquist 源码树 + 137 个二进制里都搜不到它。
;;        用 (mult 0.0 mono) 代替。
;;
;;  [致命 4] Haas 延迟: (sum (snd-const 0.0 ...) side) 并不能把 side
;;        推后。sum 只是逐点相加, 前面加一段静音等于没加。
;;        必须用 (shift-time side gap)。原版那个"出脑感"一直没生效。
;;
;;  [无效 5] 归一化: 对整个"数组"求峰值, (soundp 数组) 恒为 nil,
;;        于是 s8d-peak 永远返回 0.0, 归一化永远不执行。
;;        改为分别对左右声道调用 snd-maxsamp。
;;
;; ---------------------------------------------------------------------
;; 6 个机制的说明 (设计和上一版一致, 未改动):
;;  1. 等功率声像 cos/sin      2. Haas 延迟 (默认 18ms)
;;  3. 低频保护 (默认 200Hz)   4. 干湿比 (默认保留 25% 原声)
;;  5. BPM 卡点 + 起始相位      6. 输出归一化防削波
;;
;; 推荐档位
;;  TikTok 爆款风 : 周期 5-7 秒,  干湿 70/30, Haas 18 ms, 低频 200 Hz
;;  官方定番档   : 周期 8-12 秒 (默认 10 秒即经典值)
;;  舒缓 / 睡眠   : 周期 15-20 秒, 干湿 60/40
;;  不要低于 3 秒  : 过快会晕、不耐听
;;
;; 参考: 旋转一周 2-30 秒可调, 8-12 秒为定番, 2-4 秒过快
;;       https://web-tools.wiki-wiki.jp/guide/how-to-make-8d-audio/
;; =====================================================================


;; ============================== 用户参数 ==============================
$control PERIOD (_ "旋转周期 (秒)  6=TikTok风  10=定番") float-text "" 6.0 1.0 60.0
$control USE_BEAT (_ "用 BPM 卡点自动算周期") choice ((_ "关") (_ "开")) 0
$control BPM (_ "BPM (卡点用)") int "" 120 40 260
$control BEATS (_ "每圈拍数 (卡点用, 4=一小节)") int "" 8 1 64
$control PHASE (_ "起始相位 (度)  用来对齐重音") float-text "" 0 -360 360
$control DEPTH (_ "横扫深度 (%)  100=完全左右") int "" 100 0 100
$control WIDTH (_ "原声立体声宽度 (%)  100=原样") int "" 100 0 200
$control HAAS (_ "Haas 延迟 (毫秒)  0=关  18=推荐") float-text "" 18 0 40
$control XOVER (_ "低频保护 (Hz)  以下留在中央") float-text "" 200 0 1000
$control DRY (_ "保留原声 (%)  0=全湿") int "" 25 0 100
$control NORM (_ "输出归一化防削波 (长音频会慢)") choice ((_ "关") (_ "开")) 0
;; ====================================================================


;; ------------------------ 工具函数 ------------------------
;; 注意: 函数名不能以数字开头 (XLISP 会把 token 当数字读), 故用 s8d- 前缀。

;; 0.5*(L+R): 单声道求和, 峰值不超过 1.0, 避免相关内容翻倍削波。
(defun s8d-mono (l r)
  (mult 0.5 (sum l r)))

;; BPM 卡点: 周期 = 每圈拍数 * 60 / BPM
(defun s8d-beat-period (bpm beats fallback)
  (if (and (numberp bpm) (> bpm 0))
      (/ (* beats 60.0) bpm)
      fallback))

;; 求峰值 (用于归一化)。只能传"单个声道";
;; 传整个数组时 (soundp 数组) 是 nil, 会静默返回 0.0 让归一化失效。
(defun s8d-peak (s)
  (if (soundp s) (snd-maxsamp s) 0.0))

;; 旋转包络。用 lfo (第 1 参数是 Hz, 输出幅度 ±1), 不要用 osc。
;; 相位只在时间轴上平移旋转起点, 与波形正确性无关。
(defun s8d-base-lfo (freq dur phase)
  (lfo freq dur *sine-table* phase))

;; Haas 延时: 把声音真正推后 d 秒。
;; (sum 一段静音 s) 是逐点相加, 起不到延时作用, 必须用 shift-time。
(defun s8d-delay (s d)
  (if (> d 0) (shift-time s d) s))


;; ------------------------------ 主程序 ------------------------------
;; 立体声时 *track* 是 (vector 左声道 右声道); 单声道时不是数组。
;; Audacity 的 Nyquist 接口同时最多只支持 2 声道。
(if (not (arrayp *track*))
    (error "本效果需要立体声 (2 声道) 输入。请先 轨道 -> 混音 -> 混音并渲染, 再运行。")

  (let* (;; ---- 0. 采样率 / 长度 ----
         ;; 这里必须是全局 *sound-srate*; 写成 (snd-srate *track*)
         ;; 会因为参数是数组而抛 bad argument type, 效果直接中止。
         (sr      *sound-srate*)
         (dur     (/ len sr))
         (l-in    (aref *track* 0))
         (r-in    (aref *track* 1))

         ;; ---- 1. 周期 / 卡点 ----
         (period  (max (if (= USE_BEAT 1)
                           (s8d-beat-period BPM BEATS PERIOD)
                           PERIOD)
                       1.0))

         ;; ---- 2. 深度 (0-1) ----
         (depth   (/ DEPTH 100.0))

         ;; ---- 3. 单声道底料 ----
         (mono    (s8d-mono l-in r-in))

         ;; ---- 4. 原声干信号 (可调宽度) ----
         (w       (/ WIDTH 100.0))
         (dry     (cond
                    ((= WIDTH 100) (vector l-in r-in))
                    ((= WIDTH 0)   (vector mono mono))
                    (t             (vector
                                     (sum (mult w l-in) (mult (- 1.0 w) mono))
                                     (sum (mult w r-in) (mult (- 1.0 w) mono))))))

         ;; ---- 5. 分频: 低频留中央, 中高频参与横扫 ----
         ;; 用 hp 单独取高频, 避免 "相减" 带来的相位误差。
         (low     (if (> XOVER 0) (lp mono XOVER) (mult 0.0 mono)))
         (side    (if (> XOVER 0) (hp mono XOVER) mono))

         ;; ---- 6. Haas 延迟: 左耳提前, 右耳滞后 gap 秒 ----
         (gap     (/ (round (* HAAS (/ sr 1000.0))) sr))

         ;; ---- 7. 旋转声像 ----
         ;; panX 在 [0,1] 之间往返: 1=本侧全开  0=全给对侧
         ;; panL 与 panR 互为镜像, 所以用一个 base 取正负即可。
         (base    (s8d-base-lfo (/ 1.0 period) dur PHASE))
         (pan-l   (mult 0.5 (sum 1.0 (mult depth base))))
         (pan-r   (mult 0.5 (sum 1.0 (mult depth (mult -1.0 base)))))

         ;; ---- 8. 等功率增益: L=cos(pan*90), R=sin(pan*90) ----
         ;; 由 pan 反推: cos^2 = pan, sin^2 = 1-pan, 取正根。
         ;; 乘 1.414 让中心位置两耳各 0.707, 响度与原声一致。
         ;; pan 恒在 [0,1] 内, 所以开方不会出现负数 / NaN。
         (cos-law (mult 1.4142135 (snd-sqrt pan-l)))
         (sin-law (mult 1.4142135 (snd-sqrt pan-r)))

         ;; ---- 9. 湿信号 = 居中的低频 + 被声像的中高频 ----
         (wet-l   (sum low (mult side cos-law)))
         (wet-r   (sum low (mult (s8d-delay side gap) sin-law)))

         ;; ---- 10. 干湿混合 ----
         (dw      (/ DRY 100.0))
         (raw-l   (sum (mult (- 1.0 dw) wet-l) (mult dw (aref dry 0))))
         (raw-r   (sum (mult (- 1.0 dw) wet-r) (mult dw (aref dry 1))))

         ;; ---- 11. 归一化: 只在会削波时下压 (绝不提升, 保证 A/B 公平) ----
         ;; 必须先判断 NORM, 否则 snd-maxsamp 会把整条音频算一遍,
         ;; 4 分钟音频会吃掉几百 MB 内存。
         (pk      (if (= NORM 1) (max (s8d-peak raw-l) (s8d-peak raw-r)) 0.0))
         (gain    (if (and (= NORM 1) (> pk 0.99)) (/ 0.99 pk) 1.0))

         ;; ---- 12. 裁齐长度 ----
         ;; 右声道被 shift-time 推后了 gap 秒, 会比左声道长一点点;
         ;; 裁回选区长度, 保证左右完全等长, 也去掉那点多余尾巴。
         (out-l   (extract 0 dur (mult gain raw-l)))
         (out-r   (extract 0 dur (mult gain raw-r))))
    (vector out-l out-r)))


;; =====================================================================
;; 附: 原 "穿脑效果" 插件源码 (Brain-Panning Effect.ny), 留档备查.
;;     它只有线性音量包络, 没有等功率 / Haas / 低频保护 / 干湿比.
;;     注意它用的是 lfo (Hz), 而且用 *sound-srate* 而不是 (snd-srate *track*)
;;     —— 这两点正是本插件 2.1.0 写错的地方.
;; ---------------------------------------------------------------------
;; (setf period 10.0)
;; (setf min-vol 0.1)
;; (setf max-vol 1.0)
;; (setf mix-scale 1.0)
;; (setf center (/ (+ min-vol max-vol) 2.0))
;; (setf amp (- max-vol center))
;; (setf dur-sec (/ len *sound-srate*))
;; (if (not (arrayp *track*))
;;     (error "请选择一个立体声音轨（2声道）")
;;     (let* ((left-orig (aref *track* 0))
;;            (right-orig (aref *track* 1))
;;            (mono (mult mix-scale (sum left-orig right-orig)))
;;            (left-audio mono)
;;            (right-audio mono)
;;            (left-env (sum center (mult amp
;;              (lfo (/ 1.0 period) dur-sec *sine-table* 90))))
;;            (right-env (sum center (mult amp
;;              (lfo (/ 1.0 period) dur-sec *sine-table* 270)))))
;;       (vector (mult left-audio left-env)
;;               (mult right-audio right-env))))
;; =====================================================================
