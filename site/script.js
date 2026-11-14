const languages = {
    yack: {
        comment: /\(\*[\s\S]*?\*\)/y,
        keywords: "fun val let in end case of if then else import type datatype borrow consume mut arena and andalso orelse not true false Some None Ok Error Nil Cons",
    },
    go: { comment: /\/\/[^\n]*/y, keywords: "package import func return if else err nil var type struct for range defer go" },
    ts: { comment: /\/\/[^\n]*/y, keywords: "import from export const let function return if else async await new throw type interface null undefined string number" },
    rust: { comment: /\/\/[^\n]*/y, keywords: "use fn let mut if else while for in return match impl struct enum pub Some None String Vec self" },
    gleam: { comment: /\/\/[^\n]*/y, keywords: "import pub fn let case use try type assert todo panic Ok Error True False" },
    sh: { comment: /#[^\n]*/y, keywords: "cd curl" },
    toml: { comment: /#[^\n]*/y, keywords: "" },
};

const escapeHtml = (text) =>
    text.replace(/&/g, "&amp;").replace(/</g, "&lt;").replace(/>/g, "&gt;");

const highlight = (source, lang) => {
    const rules = languages[lang];
    if (!rules) return escapeHtml(source);
    const keywords = new Set(rules.keywords.split(" ").filter(Boolean));
    const string = /"(?:\\.|[^"\\\n])*"/y;
    const word = /[A-Za-z_][A-Za-z0-9_']*/y;
    const number = /\d+/y;
    let out = "";
    let i = 0;
    let previous = "";
    const take = (re) => {
        re.lastIndex = i;
        const m = re.exec(source);
        return m ? m[0] : null;
    };
    while (i < source.length) {
        let m;
        if ((m = take(rules.comment))) {
            out += `<span class="com">${escapeHtml(m)}</span>`;
        } else if ((m = take(string))) {
            out += `<span class="str">${escapeHtml(m)}</span>`;
        } else if ((m = take(word))) {
            const next = source[i + m.length];
            if (keywords.has(m)) out += `<span class="kw">${m}</span>`;
            else if (["fun", "func", "function", "fn"].includes(previous)) out += `<span class="fn">${m}</span>`;
            else if (/^[A-Z]/.test(m) && next === ".") out += `<span class="mod">${m}</span>`;
            else if (/^[A-Z]/.test(m)) out += `<span class="ty">${m}</span>`;
            else out += m;
            previous = m;
        } else if ((m = take(number))) {
            out += `<span class="num">${m}</span>`;
        } else {
            m = source[i];
            out += escapeHtml(m);
            if (!/\s/.test(m)) previous = m;
        }
        i += m.length;
    }
    return out;
};

document.querySelectorAll("code[data-lang]").forEach((code) => {
    code.innerHTML = highlight(code.textContent, code.dataset.lang);
});

const tocLinks = Array.from(document.querySelectorAll(".toc a"));
if (tocLinks.length && "IntersectionObserver" in window) {
    const byId = new Map(tocLinks.map((link) => [link.getAttribute("href").slice(1), link]));
    const observer = new IntersectionObserver(
        (entries) => {
            entries.forEach((entry) => {
                if (!entry.isIntersecting) return;
                tocLinks.forEach((link) => link.removeAttribute("aria-current"));
                const link = byId.get(entry.target.id);
                if (link) link.setAttribute("aria-current", "true");
            });
        },
        { rootMargin: "-30% 0px -60% 0px" },
    );
    byId.forEach((_, id) => {
        const target = document.getElementById(id);
        if (target) observer.observe(target);
    });
}

const tabGroups = document.querySelectorAll("[data-tabs]");

tabGroups.forEach((group) => {
    const tabs = Array.from(group.querySelectorAll('[role="tab"]'));
    const panels = Array.from(group.querySelectorAll('[role="tabpanel"]'));

    const select = (tab) => {
        tabs.forEach((item) => {
            const active = item === tab;
            item.setAttribute("aria-selected", String(active));
            item.tabIndex = active ? 0 : -1;
        });

        panels.forEach((panel) => {
            panel.hidden = panel.id !== tab.getAttribute("aria-controls");
        });
    };

    tabs.forEach((tab, index) => {
        tab.addEventListener("click", () => select(tab));

        tab.addEventListener("keydown", (event) => {
            let next = null;
            if (event.key === "ArrowRight") next = tabs[(index + 1) % tabs.length];
            if (event.key === "ArrowLeft") next = tabs[(index - 1 + tabs.length) % tabs.length];
            if (event.key === "Home") next = tabs[0];
            if (event.key === "End") next = tabs[tabs.length - 1];
            if (!next) return;

            event.preventDefault();
            next.focus();
            select(next);
        });
    });
});

const copyButtons = document.querySelectorAll("[data-copy]");

copyButtons.forEach((button) => {
    button.addEventListener("click", async () => {
        const card = button.closest(".code-card");
        const panel = card ? card.querySelector(".code-panel:not([hidden])") : null;
        const code = (panel || card)?.querySelector("pre code");
        if (!code) return;

        const source = code.cloneNode(true);
        source.querySelectorAll("[data-inlay]").forEach((hint) => hint.remove());

        const text = source.textContent;
        try {
            await navigator.clipboard.writeText(text);
        } catch {
            const area = document.createElement("textarea");
            area.value = text;
            area.setAttribute("readonly", "");
            area.style.position = "fixed";
            area.style.opacity = "0";
            document.body.append(area);
            area.select();
            document.execCommand("copy");
            area.remove();
        }

        const label = button.textContent;
        button.textContent = "Copied";
        window.setTimeout(() => {
            button.textContent = label;
        }, 1600);
    });
});

const tooltip = document.createElement("div");
tooltip.className = "tooltip";
tooltip.id = "code-tooltip";
tooltip.setAttribute("role", "tooltip");
document.body.append(tooltip);

const tipTargets = document.querySelectorAll("[data-tip]");
let activeTip = null;

const positionTip = (target) => {
    const rect = target.getBoundingClientRect();
    const tipRect = tooltip.getBoundingClientRect();
    const margin = 12;

    let left = rect.left + rect.width / 2 - tipRect.width / 2;
    left = Math.min(Math.max(margin, left), window.innerWidth - tipRect.width - margin);

    let top = rect.top - tipRect.height - 10;
    if (top < 8) top = rect.bottom + 10;

    tooltip.style.left = `${left}px`;
    tooltip.style.top = `${top}px`;
};

const showTip = (target) => {
    const text = target.getAttribute("data-tip");
    if (!text) return;

    activeTip = target;
    tooltip.textContent = text;
    tooltip.classList.add("is-visible");
    target.setAttribute("aria-describedby", tooltip.id);
    positionTip(target);
};

const hideTip = () => {
    if (activeTip) activeTip.removeAttribute("aria-describedby");
    activeTip = null;
    tooltip.classList.remove("is-visible");
};

tipTargets.forEach((target) => {
    target.addEventListener("mouseenter", () => showTip(target));
    target.addEventListener("mouseleave", hideTip);
    target.addEventListener("focus", () => showTip(target));
    target.addEventListener("blur", hideTip);
});

window.addEventListener("scroll", hideTip, { passive: true });
window.addEventListener("resize", hideTip);
