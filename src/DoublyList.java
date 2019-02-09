public class DoublyList {
    private int value;
    private DoublyList prev;
    private DoublyList next;
    private DoublyList tail;
    private DoublyList first;
    private DoublyList last;

    public DoublyList() {
    }

    public DoublyList(int value, DoublyList prev, DoublyList next) {
        this.value = value;
        this.prev = prev;
        this.next = next;
    }

    public DoublyList view(int... elements) {
        for (int i = elements.length - 1; i >= 0; i--) {
            tail = new DoublyList(elements[i], tail, tail);
        }
        return tail;
    }


    public void addLast(int element, DoublyList tail) {
        first = tail;
        last = null;
        if (first.next == null) {
            first.next = new DoublyList(element, first, last);
        } else {
            addLast(element, tail.next);
        }
    }

    public void addInPosition(int element, int index, DoublyList tail) {
        int count = 0;
        first = tail;
        last = tail.next;
        while (first != null) {
            if (index == 0) {
                first = null;
                last = tail;
                this.tail = new DoublyList(element, first, last);
                break;
            }
            if (index == count + 1) {
                first.next = new DoublyList(element, first, last);
                break;
            } else {
                first = first.next;
                last = last.next;
                count++;
            }
        }
    }

    public void removeLast(DoublyList tail) {
        first = tail;
        while (first.next.next != null) {
            first = first.next;
        }
        first.next = null;
    }

    public void removeByPosition(int index, DoublyList tail, int size) {
        first = tail;
        int count = 1;
        if (index == 0) {
            this.tail = first.getNext();
        }
        while (count < index) {
            first = first.getNext();
            count++;
        }
        if (count == size - 1) {
            DoublyList link = first.getNext().getNext();
            first.setNext(link);
        } else {
            DoublyList link = first.getNext().getNext();
            first.setNext(link);
            link.setPrev(first);
        }
    }

    public int size(DoublyList tail) {
        int count = 0;
        while (tail != null) {
            tail = tail.next;
            count++;
        }
        return count;
    }

    public String isGrowing(DoublyList tail, int size) {
        int count = 0;
        while (count < size - 1) {
            if (tail.value <= tail.next.value) {
                tail = tail.next;
                count++;
            } else {
                return "List isn't growing";
            }
        }
        return "List is growing";
    }

    public DoublyList getPrev() {
        return this.prev;
    }

    public void setPrev(DoublyList prev) {
        this.prev = prev;
    }

    public DoublyList getNext() {
        return next;
    }

    public void setNext(DoublyList next) {
        this.next = next;
    }

    public DoublyList getTail() {
        return tail;
    }

    public String toString(DoublyList tail) {
        return (tail == null) ? "*" : tail.value + "->" + toString(tail.next);
    }
}
