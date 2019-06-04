#include <algorithm>
#include <utility>
#include <initializer_list>

template<class T>
class Set {
private:
    struct Node {
        Node *l = nullptr, *r = nullptr, *p = nullptr;
        int h = 1;
        struct Optional {
        private:
            alignas(T) char data[sizeof(T)];
            bool is_init = 0;

        public:
            Optional(const T &x) {
                new (reinterpret_cast<T*>(data)) T(x);
                is_init = 1; 
            }
            Optional &operator=(const T &x) {
                if (is_init)
                    value().~T();
                new (reinterpret_cast<T*>(data)) T(x);
                is_init = 1; 
                return *this;
            }
            Optional() {}
            T &value() {
                return *reinterpret_cast<T*>(data);
            }
            const T &value() const {
                return *reinterpret_cast<const T*>(data);
            }
            bool has_value() const {
                return is_init;
            }
            ~Optional() {
                if (is_init)
                    value().~T();
            }
        } data;
        Node() {}
        Node(const T &data): data(data) {}
        Node(const Node &o): l(o.l), r(o.r), p(o.p), h(o.h) {
            if (o.data.has_value()) {
                data = o.data.value();
            }
        }
        void update() {
            h = 1;
            if (l) {
                l->p = this;
                h = std::max(l->h + 1, h);
            }
            if (r) {
                r->p = this;
                h = std::max(r->h + 1, h);
            }
        }
        ~Node() {
            if (l)
                delete l;
            if (r)
                delete r;
        }
        Node *copy() {
            Node *ans = new Node(*this);
            if (l) {
                ans->l = l->copy();
                ans->l->p = ans;
            }
            if (r) {
                ans->r = r->copy();
                ans->r->p = ans;
            }
            return ans;
        }
    };
    static int height(Node *t) {
        if (t)
            return t->h;
        return 0;
    }
    static int balance(Node *t) {
        if (!t)
            return 0;
        return height(t->l) - height(t->r);
    }
    static Node *small_right_rotate(Node *t) {
        Node *l = t->l;
        t->l = l->r;
        l->r = t;
        l->p = t->p;
        t->update();
        l->update();
        return l;
    }
    static Node *small_left_rotate(Node *t) {
        Node *r = t->r;
        t->r = r->l;
        r->l = t;
        r->p = t->p;
        t->update();
        r->update();
        return r;
    }
    static Node *big_right_rotate(Node *t) {
        t->l = small_left_rotate(t->l);
        return small_right_rotate(t);
    }
    static Node *big_left_rotate(Node *t) {
        t->r = small_right_rotate(t->r);
        return small_left_rotate(t);
    }
    static Node *balanced(Node *t) {
        if (balance(t) == -2)
            if (balance(t->r) > 0)
                return big_left_rotate(t);
            else
                return small_left_rotate(t);
        else if (balance(t) == 2)
            if (balance(t->l) < 0)
                return big_right_rotate(t);
            else
                return small_right_rotate(t);
        else
            return t;
    }
    static std::pair<Node *, bool> insert(Node *t, const T& x) {
        if (!t)
            return {new Node(x), 1};
        if (!(t->data.value() < x) && !(x < t->data.value()))
            return {t, 0};
        bool added;
        if (t->data.value() < x) {
            auto ins = insert(t->r, x);
            added = ins.second;
            t->r = ins.first;
            t->r->p = t;
        }
        else {
            auto ins = insert(t->l, x);
            t->l = ins.first;
            added = ins.second;
            t->l->p = t;
        }
        t->update();
        return {balanced(t), added};
    }
    static const Node *get_left(const Node *t) {
        if (!t->l)
            return t;
        return get_left(t->l);
    }
    static const Node *get_right(const Node *t) {
        if (!t->r)
            return t;
        return get_right(t->r);
    }
    static std::pair<Node*, Node*> erase(Node *t, const T &x) {
        if (!t)
            return {nullptr, nullptr};
        if (t->data.value() < x) {
            auto er = erase(t->r, x);
            t->r = er.first;
            t->update();
            t = balanced(t);
            return {t, er.second};
        }
        else if (x < t->data.value()) {
            auto er = erase(t->l, x);
            t->l = er.first;
            t->update();
            t = balanced(t);
            return {t, er.second};
        }
        else {
            if (!t->l && !t->r)
                return {nullptr, t};
            if (balance(t) >= 0) {
                auto er = erase(t->l, get_right(t->l)->data.value());
                auto e = er.second;
                e->l = er.first;
                e->r = t->r;
                e->update();
                t->l = t->r = nullptr;
                return {balanced(e), t};
            } else {
                auto er = erase(t->r, get_left(t->r)->data.value());
                auto e = er.second;
                e->r = er.first;
                e->l = t->l;
                e->update();
                t->l = t->r = nullptr;
                return {balanced(e), t};
            }
        }
    }
    static Node *lower_bound(Node *t, const T &x) {
        if (!t)
            return nullptr;
        if (t->data.value() < x)
            return lower_bound(t->r, x);
        else if (t->l) {
            auto ans = lower_bound(t->l, x);
            if (!ans)
                return t;
            else
                return ans;
        } else
            return t;
    }

    size_t _size = 0;
    Node _end;
    Node *root = nullptr;

public:
    Set() {}

    template<class It>
    Set(It b, It e) {
        while (b != e) {
            insert(*b);
            ++b;
        }
    }

    Set(const std::initializer_list<T> &l): Set(l.begin(), l.end()) {}

    Set(const Set &o) {
        _size = o._size;
        if (o.root)
            root = o.root->copy();
        _end.l = root;
        if (root)
            root->p = &_end;
    }

    Set &operator=(const Set &o) {
        if (&o == this)
            return *this;
        _size = o._size;
        if (root)
            delete root;
        if (o.root)
            root = o.root->copy();
        else
            root = nullptr;
        _end.l = root;
        if (root)
            root->p = &_end;
        return *this;
    }

    size_t size() const {
        return _size;
    }

    bool empty() const {
        return _size == 0;
    }

    void insert(const T &x) {
        auto ins = insert(root, x);
        _size += ins.second;
        root = ins.first;
        _end.l = root;
        if (root)
            root->p = &_end;
    }

    void erase(const T &x) {
        auto er = erase(root, x);
        root = er.first;
        auto e = er.second;
        if (e) {
            _size--;
            delete e;
        }
        _end.l = root;
        if (root)
            root->p = &_end;
    }

    class iterator {
    private:
        const Node *ptr;

    public:
        iterator(const Node *ptr = nullptr): ptr(ptr) {}
        bool operator==(iterator other) {
            return ptr == other.ptr;
        }
        bool operator!=(iterator other) {
            return ptr != other.ptr;
        }
        iterator &operator++() {
            if (!ptr->r) {
                while (ptr == ptr->p->r)
                    ptr = ptr->p;
                ptr = ptr->p;
            } else {
                ptr = get_left(ptr->r);
            }
            return *this;
        }
        iterator operator++(int) {
            auto ans = *this;
            ++*this;
            return ans;
        }
        iterator &operator--() {
            if (!ptr->l) {
                while (ptr == ptr->p->l)
                    ptr = ptr->p;
                ptr = ptr->p;
            } else {
                ptr = get_right(ptr->l);
            }
            return *this;
        }
        iterator operator--(int) {
            auto ans = *this;
            --*this;
            return ans;
        }
        const T *operator->() const {
            return &ptr->data.value();
        }
        const T &operator*() const {
            return ptr->data.value();
        }
    };
    iterator begin() const {
        return iterator(get_left(&_end));
    }
    iterator end() const {
        return iterator(&_end);
    }
    iterator lower_bound(const T &x) const {
        auto ans = lower_bound(root, x);
        if (!ans)
            return end();
        else
            return iterator(ans);
    }
    iterator find(const T &x) const {
        auto ans = lower_bound(root, x);
        if (!ans)
            return end();
        if (x < ans->data.value())
            return end();
        return iterator(ans);
    }
};