 rc-ext.el - .emacs.el のためのもうひとつのフレームワーク
==========================================================

 なにコレ
----------

拡張の設定書くのに、拡張がインストールされてなかったら
勝手にインストールしたり、インストールを促したりしてくれるもの。

拡張ごとの設定の記述は、ほんのすこし増えるけど
拡張について導入のしかたとか忘れないし、導入することを忘れてもいいし、
autoload の設定がやや簡単になるよ。





 依存するもの
--------------

  - cl.el ... ふつう入ってるよね
  - url.el ... 最近、ふつう入ってるよね





 あるといいもの
----------------

_rc-boot.el_

[オイラの github](https://github.com/lieutar/rc-boot.el) にあるはず。

いや、これと協働するようにできてて、要するに
この、rc-ext.el が取得してきたファイルの置き場の名前が
利用している emacsen ごとの名前になるって感じ




使いかた
--------

    ;; rc-ext をロードするよ
    (require 'rc-ext)
    
    
    ;; 普通はだいたいこんな感じ
    (rc-ext
      :load 'hoge
      :get  "http://example.com/lip/hoge.el"
      :init (lambda () (setq hoge-fuga-piyo "piyopiyo"))
    )
    
    ;; より詳しくは以下をみてね
    
    
    ;; ある問題に対し、競合するソリューションがある場合、どのソリューションを
    ;; 用いるか定義する
    (setq rc-ext-classes-alist
       '(
         (major-mode-for-foo-files . foo-x-mode)
         ;;(major-mode-for-foo-files . foo-mode)
        ))
    
    
    (rc-ext
      ;; 拡張の種類の識別名
      ;; あんまり使わないけど、競合するソリューションがある場合にこれを定義する
      :class 'major-mode-for-foo-files
    
      ;; 拡張の名前。競合するソリューションがある場合、もしくは、
      ;; 単に単一機能を requrie するだけでロードできるやーって時に
      ;; これを定義する
      :name 'foo-x-mode
    
      ;; 単に単一機能を require するときは、そのシンボルを書く
      ;; また、そのシンボルが、:name と同じなら、:name があれば、
      ;; :load はいらない
      ;; ある程度いろいろロードする必要があったり、load-library したい場合は、
      ;; lambda 式を書く
      :load (lambda ()
              (require 'foo-mode)
              (require 'foo-x-mode))
    
      ;; ロードに失敗した場合のアクションを設定する
      ;; 単一の elisp ファイルをダウンロードしてコンパイルするだけなら
      ;; URL を文字列として与える
      ;; その他の場合は、 lambda 式を記述すると、それが実行されるので、
      ;; package-install させたり、browse-url して導入を促したりすればよい
      :get  "http://example.org/lisp/foo-x-mode.el"
    
      ;; autoload を設定するコマンド名を列挙する
      ;;  (なければ、直ちにロードが試みられる)
      :autoload '(foo-ext-help foo-ext-mode)
    
      ;; 特殊な条件で読み込むなら、その判定を書く
      :cond
      (lambda () window-system)
    
      ;; 拡張を読み込む前に実行する手続きを書く
      :preload
      (lambda ()
         (add-to-list 'auto-mode-alist '("\\.foo\\'" . foo-mode)))
    
      ;; eval-after-load みたいなの
      :init
      (lambda ()
         (define-key foo-mode-map (kbd "q") 'bury-buffer)
         (defun my-foo-init ()
           (message "ほげほげ"))
         (add-hook hoo-mode-hook 'my-foo-init))
    )
    
    
    
    ;; 競合するソリューション。rc-ext-classes-alist に foo-x-mode が
    ;; major-mode-for-foo-files に設定されているのでこっちを読まないよ。
    (rc-ext
      :class 'major-mode-for-foo-files
      :name 'foo-mode
    )
    





今後
----

ぼちぼち更新するかもです。
英語とか苦手だし、typo 製造機なので命名とかに問題あるかもです。
レビュー、ダメ出しは鼻血でるほど喜びましゅ。
