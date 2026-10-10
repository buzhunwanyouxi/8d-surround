$nyquist plug-in
$version 4
$type process
$preview linear
$name (_ "初始 8D环绕")
$debugbutton disabled
$author (_ "8D环绕穿梭")
$release 2.2.0
$copyright (_ "GNU General Public License v2.0 or later")

;; =====================================================================
;; 【初始 8D环绕】  --  8D环绕穿梭 系列: 开箱即用的入门档
;; =====================================================================
;; 本档参数 (已固定, 打开即"应用", 无需调整):
;;   旋转周期 6.0 秒    = TikTok 爆款常用速度
;;   横扫深度 100%      = 完全左右穿梭
;;   原声宽度 100%      = 原声保持原样
;;   Haas 延迟 18 ms    = 出脑感 (让声音离开脑袋)
;;   低频保护 200 Hz    = 以下留中央, 鼓点不晃
;;   保留原声 25%       = 人声不发空
;;
;; 想要能调参数, 请用同子菜单下的【8D环绕穿梭】.
;; 想要别的档位, 见【爆款TikTok风】【定番10秒】【舒缓睡眠】【现场演出】.
;;
;; ---------------------------------------------------------------------
;; 2.2.0 修正 (与主插件同一批 bug, 由 Audacity 日志确认):
;;  1. (snd-srate *track*) -> *sound-srate*
;;     立体声时 *track* 是"2 个 Sound 的数组", 而 snd- 系列底层函数
;;     只能处理单声道, 于是抛 bad argument type, 效果中止, 音频原样
;;     返回 —— 这正是"波形完全没变化"的原因。
;;  2. (osc (/ 1.0 period) ...) -> (lfo (/ 1.0 period) ...)
;;     osc 的第 1 参数是"音级(step)"不是 Hz; 传 1/6 会被当成
;;     step 0.1667 = 8.257 Hz, 周期旋钮几乎失效。lfo 才是 Hz。
;;  3. (s-zeros ...) -> (mult 0.0 mono)   (S-ZEROS 这个函数不存在)
;;  4. Haas: sum 一段静音不会产生延时 -> 改用 (shift-time s gap)
;; =====================================================================


;; ------------------------ 固定预设参数 ------------------------
(setf s8d-period  6.0)      ; 旋转周期 (秒)
(setf s8d-depth   1.0)      ; 横扫深度 (1.0 = 完全左右)
(setf s8d-width   1.0)      ; 原声立体声宽度 (1.0 = 原样)
(setf s8d-haas    18.0)     ; Haas 延迟 (毫秒)
(setf s8d-xover   200.0)    ; 低频保护 (Hz)
(setf s8d-dry     0.25)     ; 保留原声 (0.25 = 25%)
;; -------------------------------------------------------------


;; ------------------------ 工具函数 ------------------------
(defun s8d-mono (l r)
  (mult 0.5 (sum l r)))

;; 旋转包络。lfo 的第 1 参数是 Hz, 输出幅度 ±1。
(defun s8d-base-lfo (freq dur phase)
  (lfo freq dur *sine-table* phase))

;; Haas 延时。sum 一段静音起不到延时作用, 必须用 shift-time。
(defun s8d-delay (s d)
  (if (> d 0) (shift-time s d) s))


;; ------------------------------ 主程序 ------------------------------
(if (not (arrayp *track*))
    (error "本效果需要立体声 (2 声道) 输入。请先 轨道 -> 混音 -> 混音并渲染, 再运行。")

  (let* ((sr      *sound-srate*)          ; 不能用 (snd-srate *track*)
         (dur     (/ len sr))
         (l-in    (aref *track* 0))
         (r-in    (aref *track* 1))

         (mono    (s8d-mono l-in r-in))

         ;; 原声干信号 (可调宽度)
         (dry     (if (= s8d-width 1.0)
                      (vector l-in r-in)
                      (vector
                        (sum (mult s8d-width l-in)
                             (mult (- 1.0 s8d-width) mono))
                        (sum (mult s8d-width r-in)
                             (mult (- 1.0 s8d-width) mono)))))

         ;; 分频: 低频留中央, 中高频参与横扫
         (low     (if (> s8d-xover 0) (lp mono s8d-xover) (mult 0.0 mono)))
         (side    (if (> s8d-xover 0) (hp mono s8d-xover) mono))

         ;; Haas 延迟: 左耳提前, 右耳滞后
         (gap     (/ (round (* s8d-haas (/ sr 1000.0))) sr))

         ;; 旋转声像: panL 与 panR 互为镜像
         (base    (s8d-base-lfo (/ 1.0 s8d-period) dur 0))
         (pan-l   (mult 0.5 (sum 1.0 (mult s8d-depth base))))
         (pan-r   (mult 0.5 (sum 1.0 (mult s8d-depth (mult -1.0 base)))))

         ;; 等功率增益: cos/sin, 乘 1.414 保持中心响度
         (cos-law (mult 1.4142135 (snd-sqrt pan-l)))
         (sin-law (mult 1.4142135 (snd-sqrt pan-r)))

         ;; 湿信号 = 居中的低频 + 被声像的中高频
         (wet-l   (sum low (mult side cos-law)))
         (wet-r   (sum low (mult (s8d-delay side gap) sin-law)))

         ;; 干湿混合
         (out-l   (sum (mult (- 1.0 s8d-dry) wet-l)
                       (mult s8d-dry (aref dry 0))))
         (out-r   (sum (mult (- 1.0 s8d-dry) wet-r)
                       (mult s8d-dry (aref dry 1)))))
    ;; 裁回选区长度 (右声道被 shift-time 推后, 会长出一点点)
    (vector (extract 0 dur out-l)
            (extract 0 dur out-r))))
